{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
    writeBuildResult,
    PkgTree (..),
    buildPkgTree,
    buildPkgSet,
  )
where

import Data.Bifunctor
import Data.Fix
import Data.Foldable (traverse_)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import GHC.Generics (Generic)
import HsNix.Builtin.AddFile
import HsNix.Builtin.FetchUrl
import HsNix.Dependent.Topo
import HsNix.Dependent.Vertex
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
import Nix.Utils
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Render.Text
import System.Directory

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
  { drvInfo :: NExpr,
    drvOutputName :: Maybe (NEL.NonEmpty Text),
    drvId :: Text,
    drvDepends :: HashSet NixDeriv
  }
  deriving (Show, Eq, Generic)

instance Hashable HsDerivation

buildDrvArg :: forall a. NamedHashAlgo a => NixDerivArg a -> NExpr
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
            case drvType d of
              InputAddressed os -> [mkOutputs os]
              FixedOutput m h ->
                [ "outputHash" $= mkStr (T.pack (show h)),
                  mkHashMode m,
                  mkHashAlgo (Proxy :: Proxy a)
                ]
              ContentAddressed m os ->
                [ mkOutputs os,
                  mkHashMode m,
                  mkHashAlgo (Proxy :: Proxy a),
                  "__contentAddressed" $= mkBool True
                ],
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
    mkOutputs os = "outputs" $= mkList (NEL.toList (fmap mkStr os))
    mkHashMode m =
      "outputHashMode"
        $= mkStr
          ( case m of
              HashFlat -> "flat"
              HashRecursive -> "recursive"
          )
    mkHashAlgo p = "outputHashAlgo" $= mkStr (hashAlgoName p)
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
        $= mkRecSet (fmap (\d -> drvId d $= drvInfo d) ds)
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

data DirDerivation = DirDrv
  { dirName :: Text,
    dirContent :: DirTree,
    dirId :: Text
  }
  deriving (Show, Eq, Generic)

instance Hashable DirDerivation

fileDir :: Path
fileDir = Path "files"

fileDirName :: Text
fileDirName = "files"

dirDrvExpr :: DirDerivation -> NExpr
dirDrvExpr f =
  Fix $
    NLiteralPath
      ( joinPath
          [ Path ".",
            fileDir,
            Path (T.unpack (dirId f)),
            Path (T.unpack (dirName f))
          ]
      )

buildDir :: [DirDerivation] -> Maybe DirTree
buildDir [] = Nothing
buildDir fs =
  Just
    (mkDir (fmap (\d -> (dirId d, mkDir [(dirName d, dirContent d)])) fs))

groupDep :: [NixDeriv] -> ([HsDerivation], [ExternalDep], [DirDerivation])
groupDep [] = ([], [], [])
groupDep (dh : dt) =
  let (h, e, f) = groupDep dt
   in case dh of
        DrvHs v -> (v : h, e, f)
        DrvExt v -> (h, v : e, f)
        DrvDir v -> (h, e, v : f)

buildDrvs :: [NixDeriv] -> (NExpr -> NExpr, NExpr, Maybe DirTree)
buildDrvs deps =
  let (hs, ext, dir) = groupDep (topoSortDep (traverse_ addVertex deps))
      (f, extB) = buildExtDep ext
   in (f, mkRecSet (catMaybes [extB, buildHsPkgs hs]), buildDir dir)

mkNixMod :: NExpr -> Text
mkNixMod = renderStrict . layoutPretty defaultLayoutOptions . prettyNix

instance DepVertex (Derivation NixDrv) where
  type EdgeSet (Derivation NixDrv) = HS.HashSet
  getDep (DrvHs h) = drvDepends h
  getDep (DrvExt _) = HS.empty
  getDep (DrvDir _) = HS.empty

newtype NixDrv a = Dep {unDep :: DepVert (HashSet (Derivation NixDrv)) a}
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
    | DrvDir DirDerivation
    deriving (Show, Eq, Generic, Hashable)
  data BuildResult NixDrv = NixMod
    { modText :: Text,
      modFiles :: Maybe (Text, DirTree)
    }
    deriving (Show)

  derivation v =
    let (drv, dep) = runDepVert (unDep v)
     in DrvHs
          ( HsDrv
              { drvInfo = buildDrvArg drv,
                drvOutputName = case drvType drv of
                  InputAddressed os -> Just os
                  FixedOutput _ _ -> Nothing
                  ContentAddressed _ os -> Just os,
                drvId = mkId (drvName drv) drv,
                drvDepends = dep
              }
          )
  storePathOf d mo =
    Dep
      ( addEdge (HS.singleton d)
          >> pure (SP (expr d))
      )
    where
      selectOut p =
        mkSelect p (maybe ["outPath"] (: ["outPath"]) mo)
      expr (DrvHs (HsDrv {drvId = i, drvOutputName = dOut})) =
        selectOut
          ( case mo of
              Just o ->
                case dOut of
                  Just os ->
                    if o `elem` os
                      then mkSym i
                      else error (concat ["Derivation ", T.unpack i, " doesn't has output ", T.unpack o])
                  Nothing -> error "Not using default output name to get store path of fixed output"
              Nothing -> mkSym i
          )
      expr (DrvExt e) = selectOut (extDepExpr e)
      expr (DrvDir f) = dirDrvExpr f

  build self@(DrvHs drv) =
    let (f, bs, d) = buildDrvs [self]
     in NixMod (mkNixMod (f (mkSelect bs [hsPkgsVar, drvId drv]))) (fmap (fileDirName,) d)
  build (DrvExt (ExtDep {extFrom = f, extPath = p})) =
    NixMod (mkNixMod (mkParamSet [(f, Nothing)] ==> mkSelect (mkSym f) p)) Nothing
  build (DrvDir _) = error "build a file store path"

writeBuildResult :: FilePath -> Text -> BuildResult NixDrv -> IO ()
writeBuildResult root n br =
  withCurrentDirectory root $ do
    TIO.writeFile (T.unpack n <> ".nix") (modText br)
    maybe (pure ()) (uncurry (writeDirTree ".")) (modFiles br)

data PkgTree
  = PkgLeaf (NEL.NonEmpty Text) (Derivation NixDrv)
  | PkgBranch (NEL.NonEmpty Text) [PkgTree]

buildPkgTree :: [PkgTree] -> BuildResult NixDrv
buildPkgTree pt =
  let (f, b, d) = buildDrvs (topoSortDep (collectDepsTL pt))
   in NixMod
        ( mkNixMod
            ( f
                ( letE
                    pkgsVar
                    b
                    ( mkWith
                        (mkSym pkgsVar)
                        (mkNonRecSet (mapMaybe buildTree pt))
                    )
                )
            )
        )
        (fmap (fileDirName,) d)
  where
    collectDepsTL = traverse_ collectDepsT
    collectDepsT (PkgLeaf _ d) = addVertex d
    collectDepsT (PkgBranch _ ds) = collectDepsTL ds

    pkgsVar = "pkgs"

    mkBinding k v =
      NamedVar
        (StaticKey . VarName <$> k)
        v
        (SourcePos "<generated>" (mkPos 1) (mkPos 1))
    buildTree (PkgLeaf p d) =
      fmap
        (mkBinding p)
        ( case d of
            DrvHs dh -> Just $ mkSelect hsPkgsSym [drvId dh]
            DrvExt e -> Just $ extDepExpr e
            DrvDir _ -> Nothing
        )
    buildTree (PkgBranch p ts) =
      Just $ mkBinding p (mkNonRecSet (mapMaybe buildTree ts))

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
            drvType =
              FixedOutput
                (if isExecutable fa || unpack fa then HashRecursive else HashFlat)
                (outputHash fa)
          }
    where
      fromBool v = if v then "1" else ""

instance BuiltinAddText NixDrv where
  addTextFile n c =
    let d = textFile NonExecutable c
     in storePath
          ( DrvDir
              ( DirDrv
                  { dirName = n,
                    dirContent = d,
                    dirId = mkId n (n, d)
                  }
              )
          )

instance BuiltinAddDir NixDrv where
  addDirectory n d =
    storePath
      ( DrvDir
          ( DirDrv
              { dirName = n,
                dirContent = d,
                dirId = mkId n (n, d)
              }
          )
      )