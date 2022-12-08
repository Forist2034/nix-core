{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Derivation.NixDrv
  ( -- reexport Derivation
    NixDerivArg,
    module D,
    NixStr,
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
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Nix.Atoms
import Nix.Builtin.FetchUrl
import Nix.Derivation
import Nix.Derivation as D hiding (BuildResult, Derivation, StorePath)
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

mkSym :: Text -> NExpr
mkSym = Fix . NSym . VarName

mkStr :: Text -> NExpr
mkStr s = Fix $ NStr (DoubleQuoted [Plain s])

mkList :: [NExpr] -> NExpr
mkList = wrapFix . NList

mkBind :: Text -> r -> Binding r
mkBind k v = NamedVar (NEL.fromList [StaticKey (VarName k)]) v (SourcePos "generated" (mkPos 1) (mkPos 1))

mkBool :: Bool -> NExpr
mkBool v = Fix $ NConstant (NBool v)

mkAbs :: [Text] -> NExpr -> NExpr
mkAbs arg = wrapFix . NAbs (ParamSet Nothing Closed (fmap (\a -> (VarName a, Nothing)) arg))

mkBuiltin :: Text -> NExpr
mkBuiltin v = mkSelect (mkSym "builtins") [v]

mkCall :: NExpr -> NExpr -> NExpr
mkCall a v = Fix (NBinary NApp a v)

mkName :: (Hashable a) => Text -> a -> Text
mkName base v =
  base
    <> "_"
    <> T.pack (showHex (fromIntegral (hash v) :: Word) "")

type NixDerivArg = DerivationArg NixStr (Derivation NixDrv)

-- | string components (in reverse order)
newtype NixStr = NixStr [Antiquoted Text NExpr]
  deriving (Show, Eq, Generic)

instance Hashable NixStr

instance IsString NixStr where
  fromString s = NixStr [Plain (T.pack s)]
  {-# INLINE fromString #-}

instance Semigroup NixStr where
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

instance Monoid NixStr where
  mempty = NixStr []

instance IsDrvStr NixStr where
  fromText t = NixStr [Plain t]
  {-# INLINE fromText #-}

strToExpr :: NixStr -> NExpr
strToExpr (NixStr f) = wrapFix $ NStr (DoubleQuoted (reverse f))

buildDrvArg :: DerivationArg NixStr (Derivation NixDrv) -> NExpr
buildDrvArg d =
  mkCall
    (mkBuiltin "derivation")
    ( wrapFix $
        NSet
          NonRecursive
          ( fmap
              (uncurry mkBind)
              ( concat
                  [ [ ("name", mkStr (drvName d)),
                      ("builder", strToExpr (drvBuilder d)),
                      ("system", mkStr (systemName (drvSystem d)))
                    ],
                    [("args", mkList (strToExpr <$> drvArgs d)) | not (null (drvArgs d))],
                    fmap (second strToExpr) (drvEnv d ++ drvPassAsFile d),
                    [ ("passAsFile", mkList (mkStr . fst <$> drvPassAsFile d))
                      | not (null (drvPassAsFile d))
                    ],
                    [("outputs", mkList (mkStr <$> drvOutputs d)) | not (null (drvOutputs d))],
                    [ ( "outputHashMode",
                        mkStr
                          ( case drvHashMode d of
                              HashFlat -> "flat"
                              HashRecursive -> "recursive"
                          )
                      ),
                      ( "outputHashAlgo",
                        mkStr
                          ( case drvHashAlgo d of
                              HashSha1 -> "sha1"
                              HashSha256 -> "sha256"
                              HashSha512 -> "sha512"
                          )
                      )
                    ],
                    maybeField
                      ( \case
                          Hash h -> [("outputHash", mkStr h)]
                      )
                      (drvHash d),
                    depField "allowedReferences" (drvAllowedReferences d),
                    depField "allowedRequisites" (drvAllowedRequisites d),
                    depField "disallowedReferences" (drvDisallowedReferences d),
                    depField "disallowedRequisites" (drvDisallowedRequisites d),
                    [ ("preferLocalBuild", mkBool (drvPreferLocalBuild d)),
                      ("allowSubstitutes", mkBool (drvAllowSubstitutes d))
                    ]
                  ]
              )
          )
    )
  where
    drvExpr (DrvHs (HsDrv {drvId = i})) = mkSym i
    drvExpr (DrvExt (ExtDep {extPath = i, extFrom = f})) = mkSelect (mkSym f) i

    maybeField = maybe []
    depField n = maybeField (\f -> [(n, mkList (fmap drvExpr f))])

type Deps = (HashSet HsDerivation, HashSet Text)

mergeDep :: Deps -> Deps -> Deps
mergeDep (ab, ad) (bb, bd) = (HS.union ab bb, HS.union ad bd)

collectDeps :: Derivation NixDrv -> Deps
collectDeps (DrvHs self@(HsDrv {drvDepends = dep})) =
  HS.foldr (mergeDep . collectDeps) (HS.singleton self, HS.empty) dep
collectDeps (DrvExt (ExtDep {extFrom = f})) = (HS.empty, HS.singleton f)

data HsDerivation = HsDrv
  { drvInfo :: DerivationArg NixStr (Derivation NixDrv),
    drvId :: Text,
    drvDepends :: HashSet (Derivation NixDrv)
  }
  deriving (Show, Eq, Generic)

instance Hashable HsDerivation

buildHsDrv :: HsDerivation -> Binding NExpr
buildHsDrv d = mkBind (drvId d) (buildDrvArg (drvInfo d))

data ExternalDep = ExtDep {extFrom :: Text, extPath :: [Text]}
  deriving (Show, Eq, Generic)

instance Hashable ExternalDep

externalDep :: Text -> [Text] -> Derivation NixDrv
externalDep from = DrvExt . ExtDep from

extDepExpr :: ExternalDep -> NExpr
extDepExpr e = mkSelect (mkSym (extFrom e)) (extPath e)

-- | build expr like {dep1, dep2, .. depn} : body
extDepAbs :: [Text] -> NExpr -> NExpr
extDepAbs ext body = if null ext then body else mkAbs ext body

mkNixMod :: NExpr -> BuildResult NixDrv
mkNixMod = NixMod . renderStrict . layoutPretty defaultLayoutOptions . prettyNix

newtype NixDrv a = Dep {unDep :: Writer (HashSet (Derivation NixDrv)) a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadDeriv NixDrv where
  type DrvStr NixDrv = NixStr
  newtype StorePath NixDrv = SP {pathStr :: NixStr}
    deriving (Eq, Show)
    deriving newtype (Hashable)
  data Derivation NixDrv
    = DrvHs HsDerivation
    | DrvExt ExternalDep
    deriving (Show, Eq, Generic, Hashable)
  newtype BuildResult NixDrv = NixMod {modText :: Text}
    deriving (Show)

  derivation v =
    let (drv, dep) = runWriter (unDep v)
     in DrvHs
          ( HsDrv
              { drvInfo = drv,
                drvId = mkName (drvName drv) drv,
                drvDepends = dep
              }
          )
  addFile n v =
    SP $
      NixStr
        [ Antiquoted $
            mkCall (mkCall (mkBuiltin "toFile") (mkStr n)) (mkStr v)
        ]
  pathToStr = pathStr
  storePath d o =
    Dep
      ( tell (HS.singleton d)
          >> return
            ( SP $
                NixStr
                  [ Antiquoted
                      ( mkSelect
                          ( case d of
                              DrvHs (HsDrv {drvId = i, drvInfo = info}) ->
                                if o `elem` drvOutputs info
                                  then mkSym i
                                  else error (concat ["Derivation ", T.unpack i, " doesn't has output ", T.unpack o])
                              DrvExt e -> extDepExpr e
                          )
                          [o, "outPath"]
                      )
                  ]
            )
      )
  build (DrvHs drv) =
    let (builds, extern) = collectDeps (DrvHs drv)
        body =
          mkSelect
            (Fix $ NSet Recursive (buildHsDrv <$> HS.toList builds))
            [drvId drv]
     in mkNixMod (extDepAbs (HS.toList extern) body)
  build (DrvExt e@(ExtDep {extFrom = f})) =
    mkNixMod (mkAbs [f] (extDepExpr e))

data PkgTree
  = PkgLeaf (NEL.NonEmpty Text) (Derivation NixDrv)
  | PkgBranch (NEL.NonEmpty Text) [PkgTree]

buildPkgTree :: [PkgTree] -> BuildResult NixDrv
buildPkgTree pt =
  mkNixMod
    ( extDepAbs
        (HS.toList externs)
        ( wrapFix $
            NLet
              [mkBind hsPkg hsBody]
              (Fix $ NSet NonRecursive (fmap buildTree pt))
        )
    )
  where
    collectDepsTL = foldr (\i d -> mergeDep d (collectDepsT i)) (HS.empty, HS.empty)
    collectDepsT (PkgLeaf _ d) = collectDeps d
    collectDepsT (PkgBranch _ ds) = collectDepsTL ds

    (builds, externs) = collectDepsTL pt
    hsBody = Fix $ NSet Recursive (buildHsDrv <$> HS.toList builds)
    hsPkg = mkName "pkgs_hs" hsBody
    pkgSym = mkSym hsPkg

    mkBinding k v =
      NamedVar
        (StaticKey . VarName <$> k)
        v
        (SourcePos "<generated>" (mkPos 1) (mkPos 1))
    buildTree (PkgLeaf p d) =
      mkBinding
        p
        ( case d of
            DrvHs dh -> mkSelect pkgSym [drvId dh]
            DrvExt e -> extDepExpr e
        )
    buildTree (PkgBranch p ts) =
      mkBinding p (Fix $ NSet NonRecursive (buildTree <$> ts))

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