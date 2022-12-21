{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HsNix.Derivation.NixDrv
  ( -- reexport Derivation
    NixDerivArg,
    module D,
    externalDep,
    NixDrv,
    DrvStr,
    DrvStrBuilder,
    StorePath,
    Derivation,
    BuildResult (..),
    PkgTree (..),
    buildPkgTree,
    buildPkgSet,
  )
where

import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import Data.Fix
import Data.Foldable (traverse_)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (catMaybes)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import GHC.Generics (Generic)
import HsNix.Builtin.AddFile
import HsNix.Builtin.FetchUrl
import HsNix.Derivation
import HsNix.Derivation as D hiding
  ( BuildResult,
    Derivation,
    DrvStr,
    DrvStrBuilder,
    StorePath,
  )
import HsNix.Hash
import HsNix.Internal.System
import Nix.Expr.Shorthands
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import Nix.Pretty
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Render.Text

mkSelect :: NExpr -> [Text] -> NExpr
mkSelect f t =
  Fix $
    NSelect
      Nothing
      f
      (NEL.fromList (StaticKey . VarName <$> t))

mkBuiltin :: Text -> NExpr
mkBuiltin v = mkSym "builtins" @. v

mkId :: (Hashable a) => Text -> a -> Text
mkId base v =
  base
    <> "_"
    <> T.pack (showHex (fromIntegral (hash v) :: Word) "")

type NixDerivArg = DerivationArg NixDrv

type NixStr = DrvStr NixDrv

data DrvStrSpan
  = Str Text
  | Drv NExpr
  deriving (Eq, Show, Generic)

instance Hashable DrvStrSpan

instance IsString NixStr where
  fromString s = DStr [Str (fromString s)]
  {-# INLINE fromString #-}

instance AsDrvStr Text NixStr where
  toDrvStr s = DStr [Str s]

instance AsDrvStr (StorePath NixDrv) NixStr where
  toDrvStr (SP s) = DStr [Drv s]

instance Semigroup NixStr where
  DStr [] <> r = r
  l <> DStr [] = l
  DStr l <> DStr r@(r1 : rs) =
    let ll = last l
     in DStr
          ( case (ll, r1) of
              (Str sl, Str sr) -> init l ++ Str (sl <> sr) : rs
              _ -> l ++ r
          )

instance Monoid NixStr where
  mempty = DStr []

data SpanBuilder
  = SpanStr LTB.Builder
  | SpanDrv NExpr

instance Semigroup (DrvStrBuilder NixDrv) where
  DB l <> DB r = DB (l . r)

instance Monoid (DrvStrBuilder NixDrv) where
  mempty = DB id

instance IsString (DrvStrBuilder NixDrv) where
  fromString s = DB (SpanStr (fromString s) :)
  {-# INLINE fromString #-}

instance AsDrvStr (DrvStrBuilder NixDrv) NixStr where
  toDrvStr (DB db) =
    DStr
      ( fmap
          ( \case
              SpanStr s -> Str (LT.toStrict (LTB.toLazyText s))
              SpanDrv d -> Drv d
          )
          (mergeT (db []))
      )
    where
      mergeT [] = []
      mergeT (d@(SpanDrv _) : ps) = d : mergeT ps
      mergeT (SpanStr s : ps) =
        case mergeT ps of
          (SpanStr ss : pss) -> SpanStr (s <> ss) : pss
          v -> SpanStr s : v

instance HasStrBuilder NixDrv where
  newtype DrvStrBuilder NixDrv = DB ([SpanBuilder] -> [SpanBuilder])
  fromDrvStr (DStr s) =
    DB
      ( \back ->
          foldr
            ( \i ss -> case i of
                Str st -> SpanStr (LTB.fromText st) : ss
                Drv d -> SpanDrv d : ss
            )
            back
            s
      )
  quote q (DStr st) =
    foldMap
      ( \case
          Str s -> quoteStr s
          Drv d -> [QStr (DStr [Drv d])]
      )
      st
    where
      quoteStr s
        | T.null s = []
        | otherwise =
            let (h, t) = T.break q s
             in case T.uncons t of
                  Just (esc, ts) -> QStr (DStr [Str h]) : QEscape esc : quoteStr ts
                  Nothing -> [QStr (DStr [Str h])]

strToExpr :: DrvStr NixDrv -> NExpr
strToExpr (DStr f) =
  wrapFix $
    NStr
      ( DoubleQuoted
          ( fmap
              ( \case
                  Str s -> Plain s
                  Drv e -> Antiquoted e
              )
              f
          )
      )

type NixDeriv = Derivation NixDrv

data HsDerivation = HsDrv
  { drvInfo :: NixDerivArg,
    drvId :: Text,
    drvDepends :: HashSet NixDeriv
  }
  deriving (Show, Eq, Generic)

instance Hashable HsDerivation

buildDrvArg :: NixDerivArg -> NExpr
buildDrvArg d =
  mkBuiltin "derivation"
    @@ mkNonRecSet
      ( concat
          [ [ "name" $= mkStr (drvName d),
              "builder" $= strToExpr (drvBuilder d),
              "system" $= mkStr (systemName (drvSystem d))
            ],
            ["args" $= mkList (strToExpr <$> drvArgs d) | not (null (drvArgs d))],
            fmap (uncurry bindTo . second strToExpr) (drvEnv d ++ drvPassAsFile d),
            [ "passAsFile" $= mkList (mkStr . fst <$> drvPassAsFile d)
              | not (null (drvPassAsFile d))
            ],
            ["outputs" $= mkList (NEL.toList (mkStr <$> drvOutputs d)) | not (null (drvOutputs d))],
            [ "outputHashMode"
                $= mkStr
                  ( case drvHashMode d of
                      HashFlat -> "flat"
                      HashRecursive -> "recursive"
                  ),
              "outputHashAlgo"
                $= mkStr
                  ( case drvHashAlgo d of
                      HashSha1 -> "sha1"
                      HashSha256 -> "sha256"
                      HashSha512 -> "sha512"
                  )
            ],
            maybeField
              ( \case
                  Hash h -> ["outputHash" $= mkStr h]
              )
              (drvHash d),
            depField "allowedReferences" (drvAllowedReferences d),
            depField "allowedRequisites" (drvAllowedRequisites d),
            depField "disallowedReferences" (drvDisallowedReferences d),
            depField "disallowedRequisites" (drvDisallowedRequisites d),
            [ "preferLocalBuild" $= mkBool (drvPreferLocalBuild d),
              "allowSubstitutes" $= mkBool (drvAllowSubstitutes d)
            ]
          ]
      )
  where
    maybeField = maybe []
    depField n = maybeField (\f -> [n $= mkList (fmap strToExpr f)])

hsPkgsVar :: Text
hsPkgsVar = "hs-packages"

hsPkgsSym :: NExpr
hsPkgsSym = mkSym hsPkgsVar

buildHsPkgs :: [HsDerivation] -> Maybe (Binding NExpr)
buildHsPkgs [] = Nothing
buildHsPkgs ds =
  Just
    ( hsPkgsVar
        $= mkRecSet (fmap (\d -> drvId d $= buildDrvArg (drvInfo d)) ds)
    )

data ExternalDep = ExtDep {extFrom :: Text, extPath :: [Text], extId :: Text}
  deriving (Show, Eq, Generic)

instance Hashable ExternalDep

externalDep :: Text -> [Text] -> NixDeriv
externalDep from path =
  DrvExt
    ( ExtDep
        { extFrom = from,
          extPath = path,
          extId =
            mkId (T.intercalate (T.singleton '.') (from : path)) (from : path)
        }
    )

extDepVar :: Text
extDepVar = "external"

extDepSym :: NExpr
extDepSym = mkSym extDepVar

extDepExpr :: ExternalDep -> NExpr
extDepExpr e = extDepSym @. extId e

buildExtDep :: [ExternalDep] -> (NExpr -> NExpr, Maybe (Binding NExpr))
buildExtDep [] = (id, Nothing)
buildExtDep e =
  ( \body -> Param (VarName extDepAbs) ==> body,
    Just
      ( extDepVar
          $= mkNonRecSet
            ( fmap
                ( \d ->
                    extId d $= mkSelect extDepAbsSym (extFrom d : extPath d)
                )
                e
            )
      )
  )
  where
    extDepAbs :: Text
    extDepAbs = "external_package"

    extDepAbsSym = mkSym extDepAbs

data FileDerivation = FileDrv
  { fileName :: Text,
    fileContent :: Text,
    fileId :: Text
  }
  deriving (Show, Eq, Generic)

instance Hashable FileDerivation

fileVar :: Text
fileVar = "file"

filesSym :: NExpr
filesSym = mkSym fileVar

fileDrvExpr :: FileDerivation -> NExpr
fileDrvExpr f = filesSym @. fileId f

fileExpr :: FileDerivation -> NExpr
fileExpr f = mkBuiltin "toFile" @@ mkStr (fileName f) @@ mkStr (fileContent f)

buildFileDrv :: [FileDerivation] -> Maybe (Binding NExpr)
buildFileDrv [] = Nothing
buildFileDrv fs =
  Just
    ( fileVar
        $= mkNonRecSet
          (fmap (\f -> fileId f $= fileExpr f) fs)
    )

type DepM = State (HashSet NixDeriv)

runDepM :: DepM a -> [NixDeriv]
runDepM mv = HS.toList (execState mv HS.empty)

collectDepsL :: Foldable f => f (Derivation NixDrv) -> DepM ()
collectDepsL = traverse_ collectDeps

collectDeps :: Derivation NixDrv -> DepM ()
collectDeps self@(DrvHs (HsDrv {drvDepends = dep})) =
  gets (HS.member self) >>= \case
    True -> return ()
    False ->
      modify (HS.insert self)
        >> collectDepsL dep
collectDeps v = modify (HS.insert v)

groupDep :: [NixDeriv] -> ([HsDerivation], [ExternalDep], [FileDerivation])
groupDep [] = ([], [], [])
groupDep (dh : dt) =
  let (h, e, f) = groupDep dt
   in case dh of
        DrvHs v -> (v : h, e, f)
        DrvExt v -> (h, v : e, f)
        DrvFile v -> (h, e, v : f)

buildDrvs :: [NixDeriv] -> (NExpr -> NExpr, NExpr)
buildDrvs deps =
  let (hs, ext, file) = groupDep (runDepM (collectDepsL deps))
      (f, extB) = buildExtDep ext
   in (f, mkRecSet (catMaybes [extB, buildFileDrv file, buildHsPkgs hs]))

mkNixMod :: NExpr -> BuildResult NixDrv
mkNixMod = NixMod . renderStrict . layoutPretty defaultLayoutOptions . prettyNix

newtype NixDrv a = Dep {unDep :: Writer (HashSet (Derivation NixDrv)) a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadDeriv NixDrv where
  newtype DrvStr NixDrv = DStr [DrvStrSpan]
    deriving (Eq, Show)
    deriving newtype (Hashable)
  newtype StorePath NixDrv = SP {pathExp :: NExpr}
    deriving (Eq, Show)
    deriving newtype (Hashable)
  data Derivation NixDrv
    = DrvHs HsDerivation
    | DrvExt ExternalDep
    | DrvFile FileDerivation
    deriving (Show, Eq, Generic, Hashable)
  newtype BuildResult NixDrv = NixMod {modText :: Text}
    deriving (Show)

  derivation v =
    let (drv, dep) = runWriter (unDep v)
     in DrvHs
          ( HsDrv
              { drvInfo = drv,
                drvId = mkId (drvName drv) drv,
                drvDepends = dep
              }
          )
  storePathOf d mo =
    Dep
      ( tell (HS.singleton d)
          >> return
            (SP (expr d))
      )
    where
      selectOut p =
        mkSelect p (maybe ["outPath"] (: ["outPath"]) mo)
      expr (DrvHs (HsDrv {drvId = i, drvInfo = info})) =
        selectOut
          ( case mo of
              Just o ->
                if o `elem` drvOutputs info
                  then mkSym i
                  else error (concat ["Derivation ", T.unpack i, " doesn't has output ", T.unpack o])
              Nothing -> mkSym i
          )
      expr (DrvExt e) = selectOut (extDepExpr e)
      expr (DrvFile f) = fileDrvExpr f

  build self@(DrvHs drv) =
    let (f, bs) = buildDrvs [self]
     in mkNixMod (f (mkSelect bs [hsPkgsVar, drvId drv]))
  build (DrvExt (ExtDep {extFrom = f, extPath = p})) =
    mkNixMod (mkParamSet [(f, Nothing)] ==> mkSelect (mkSym f) p)
  build (DrvFile f) =
    mkNixMod (fileExpr f)

data PkgTree
  = PkgLeaf (NEL.NonEmpty Text) (Derivation NixDrv)
  | PkgBranch (NEL.NonEmpty Text) [PkgTree]

buildPkgTree :: [PkgTree] -> BuildResult NixDrv
buildPkgTree pt =
  mkNixMod
    ( let (f, b) = buildDrvs (runDepM (collectDepsTL pt))
       in f
            ( letE
                pkgsVar
                b
                ( mkWith
                    (mkSym pkgsVar)
                    (mkNonRecSet (fmap buildTree pt))
                )
            )
    )
  where
    collectDepsTL = traverse_ collectDepsT
    collectDepsT (PkgLeaf _ d) = collectDeps d
    collectDepsT (PkgBranch _ ds) = collectDepsTL ds

    pkgsVar = "pkgs"

    mkBinding k v =
      NamedVar
        (StaticKey . VarName <$> k)
        v
        (SourcePos "<generated>" (mkPos 1) (mkPos 1))
    buildTree (PkgLeaf p d) =
      mkBinding
        p
        ( case d of
            DrvHs dh -> mkSelect hsPkgsSym [drvId dh]
            DrvExt e -> extDepExpr e
            DrvFile f -> fileDrvExpr f
        )
    buildTree (PkgBranch p ts) =
      mkBinding p (mkNonRecSet (buildTree <$> ts))

buildPkgSet :: [(Text, Derivation NixDrv)] -> BuildResult NixDrv
buildPkgSet = buildPkgTree . fmap (\(i, d) -> PkgLeaf (NEL.singleton i) d)

instance BuiltinFetchUrl NixDrv where
  fetchUrl fa =
    derivation $
      return
        (defaultDrvArg (name fa) "builtin:fetchurl" (System "builtin"))
          { drvEnv =
              [ ("url", toDrvStr (url fa)),
                ("executable", fromBool (isExecutable fa)),
                ("unpack", fromBool (unpack fa)),
                ("urls", toDrvStr (url fa))
              ],
            drvPreferLocalBuild = True,
            drvHash = Just (outputHash fa),
            drvHashMode = if isExecutable fa || unpack fa then HashRecursive else HashFlat,
            drvHashAlgo = hashAlgo fa
          }
    where
      fromBool v = if v then "1" else ""

instance BuiltinAddText NixDrv where
  addTextFile n c =
    storePath
      ( DrvFile
          ( FileDrv
              { fileName = n,
                fileContent = c,
                fileId = mkId n (n, c)
              }
          )
      )