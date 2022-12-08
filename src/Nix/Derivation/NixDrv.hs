{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Nix.Derivation.NixDrv
  ( -- reexport Derivation
    NixDerivArg,
    module D,
    externalDep,
    NixDrv,
    StorePath,
    Derivation,
    BuildResult (..),
    PkgTree (..),
    buildPkgTree,
    buildPkgSet,
  )
where

import Control.Monad.Writer
import Data.Bifunctor
import Data.Fix
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (catMaybes)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Nix.Builtin.AddFile
import Nix.Builtin.FetchUrl
import Nix.Derivation
import Nix.Derivation as D hiding (BuildResult, Derivation, StorePath)
import Nix.Expr.Shorthands
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import Nix.Hash
import Nix.Internal.System
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

instance IsString (DrvStr NixDrv) where
  fromString s = NixStr [Plain (T.pack s)]
  {-# INLINE fromString #-}

instance Semigroup (DrvStr NixDrv) where
  NixStr l@(l1 : ls) <> NixStr r@(_ : _) =
    let r1 = last r
        rs = init r
     in NixStr
          ( case (l1, r1) of
              (Plain pl, Plain pr) -> rs ++ (Plain (pl <> pr) : ls)
              _ -> r ++ l
          )
  NixStr [] <> NixStr r = NixStr r
  NixStr l <> NixStr [] = NixStr l

instance Monoid (DrvStr NixDrv) where
  mempty = NixStr []

instance IsDrvStr (DrvStr NixDrv) where
  fromText t = NixStr [Plain t]
  {-# INLINE fromText #-}

strToExpr :: DrvStr NixDrv -> NExpr
strToExpr (NixStr f) = wrapFix $ NStr (DoubleQuoted (reverse f))

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
    drvExpr (DrvHs (HsDrv {drvId = i})) = mkSym i
    drvExpr (DrvExt e) = extDepExpr e
    drvExpr (DrvFile f) = fileDrvExpr f

    maybeField = maybe []
    depField n = maybeField (\f -> [n $= mkList (fmap drvExpr f)])

hsPkgsVar :: Text
hsPkgsVar = "hs-packages"

hsPkgsSym :: NExpr
hsPkgsSym = mkSym hsPkgsVar

buildHsPkgs :: [HsDerivation] -> Maybe (Binding NExpr)
buildHsPkgs [] = Nothing
buildHsPkgs ds =
  Just
    ( hsPkgsVar
        $= mkNonRecSet (fmap (\d -> drvId d $= buildDrvArg (drvInfo d)) ds)
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

data FileDerivation = FileDrv {fileName, fileContent :: Text, fileId :: Text}
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

collectDepsL :: (Foldable f) => HashSet NixDeriv -> f NixDeriv -> HashSet NixDeriv
collectDepsL = foldr (HS.union . collectDeps)

collectDeps :: Derivation NixDrv -> HashSet NixDeriv
collectDeps self@(DrvHs (HsDrv {drvDepends = dep})) =
  collectDepsL (HS.singleton self) dep
collectDeps v = HS.singleton v

groupDep :: [NixDeriv] -> ([HsDerivation], [ExternalDep], [FileDerivation])
groupDep [] = ([], [], [])
groupDep (dh : dt) =
  let (h, e, f) = groupDep dt
   in case dh of
        DrvHs v -> (v : h, e, f)
        DrvExt v -> (h, v : e, f)
        DrvFile v -> (h, e, v : f)

buildDrvs :: [NixDeriv] -> (NExpr -> NExpr, [Binding NExpr])
buildDrvs deps =
  let (hs, ext, file) = groupDep (HS.toList (collectDepsL HS.empty deps))
      (f, extB) = buildExtDep ext
   in (f, catMaybes [extB, buildFileDrv file, buildHsPkgs hs])

mkNixMod :: NExpr -> BuildResult NixDrv
mkNixMod = NixMod . renderStrict . layoutPretty defaultLayoutOptions . prettyNix

newtype NixDrv a = Dep {unDep :: Writer (HashSet (Derivation NixDrv)) a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadDeriv NixDrv where
  -- string components (in reverse order)
  newtype DrvStr NixDrv = NixStr [Antiquoted Text NExpr]
    deriving (Show, Eq, Generic)
    deriving newtype (Hashable)
  newtype StorePath NixDrv = SP {pathStr :: DrvStr NixDrv}
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
  pathToStr = pathStr
  storePathOf d mo =
    Dep
      ( tell (HS.singleton d)
          >> return
            (SP $ NixStr [Antiquoted (expr d)])
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
     in mkNixMod (f (mkSelect (mkRecSet bs) [hsPkgsVar, drvId drv]))
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
    ( let (f, b) = buildDrvs (HS.toList (collectDepsTL pt))
       in f (mkLets b (mkNonRecSet (fmap buildTree pt)))
    )
  where
    collectDepsTL = foldr (HS.union . collectDepsT) HS.empty
    collectDepsT (PkgLeaf _ d) = collectDeps d
    collectDepsT (PkgBranch _ ds) = collectDepsTL ds

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
              [ ("url", fromText (url fa)),
                ("executable", fromBool (isExecutable fa)),
                ("unpack", fromBool (unpack fa)),
                ("urls", fromText (url fa))
              ],
            drvPreferLocalBuild = True,
            drvHash = Just (outputHash fa),
            drvHashMode = if isExecutable fa || unpack fa then HashRecursive else HashFlat,
            drvHashAlgo = hashAlgo fa
          }
    where
      fromBool v = if v then "1" else ""

instance BuiltinAddFile NixDrv where
  addFile n c =
    storePath
      ( DrvFile
          ( FileDrv
              { fileName = n,
                fileContent = c,
                fileId = mkId n (n, c)
              }
          )
      )