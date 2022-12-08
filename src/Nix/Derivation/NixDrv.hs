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
    (</>),
    externalDep,
    NixDrv,
    StorePath,
    Derivation,
    BuildResult (..),
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
import Nix.Derivation
import Nix.Derivation as D hiding (BuildResult, Derivation, StorePath) -- for reexport
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import Nix.Hash
import Nix.Internal.System (systemName)
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

type NixDerivArg = DerivationArg NixStr (Derivation NixDrv)

-- | string components (in reverse order)
newtype NixStr = NixStr [Antiquoted Text NExpr]
  deriving (Show, Eq, Generic)

instance Hashable NixStr

instance IsString NixStr where
  fromString s = NixStr [Plain (fromString s)]

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

(</>) :: NixStr -> NixStr -> NixStr
l </> r = l <> "/" <> r

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
                    fmap
                      ( second
                          ( \case
                              DrvHs (HsDrv {drvId = i}) -> mkStr i
                              DrvExt (ExtDep {extPath = i, extFrom = f}) -> mkSelect (mkSym f) i
                          )
                      )
                      (drvDependent d),
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
                    maybe
                      []
                      ( \case
                          Hash h -> [("outputHash", mkStr h)]
                      )
                      (drvHash d),
                    [ ("preferLocalBuild", mkBool (drvPreferLocalBuild d)),
                      ("allowSubstitutes", mkBool (drvAllowSubstitutes d))
                    ]
                  ]
              )
          )
    )

collectDeps :: Derivation NixDrv -> (HashSet HsDerivation, HashSet Text)
collectDeps (DrvHs self@(HsDrv {drvDepends = dep})) =
  HS.foldr (mergeDep . collectDeps) (HS.singleton self, HS.empty) dep
  where
    mergeDep (ab, ad) (bb, bd) = (HS.union ab bb, HS.union ad bd)
collectDeps (DrvExt (ExtDep {extFrom = f})) = (HS.empty, HS.singleton f)

data HsDerivation = HsDrv
  { drvInfo :: DerivationArg NixStr (Derivation NixDrv),
    drvId :: Text,
    drvDepends :: HashSet (Derivation NixDrv)
  }
  deriving (Show, Eq, Generic)

instance Hashable HsDerivation

data ExternalDep = ExtDep {extFrom :: Text, extPath :: [Text]}
  deriving (Show, Eq, Generic)

instance Hashable ExternalDep

externalDep :: Text -> [Text] -> Derivation NixDrv
externalDep from = DrvExt . ExtDep from

mkNixMod :: NExpr -> BuildResult NixDrv
mkNixMod = NixMod . renderStrict . layoutPretty defaultLayoutOptions . prettyNix

newtype NixDrv a = Dep {unDep :: Writer (HashSet (Derivation NixDrv)) a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadDeriv NixDrv where
  type DrvStr NixDrv = NixStr
  newtype StorePath NixDrv = SP {pathStr :: NixStr}
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
                drvId =
                  drvName drv
                    <> "_"
                    <> T.pack (showHex (fromIntegral (hash drv) :: Word) ""),
                drvDepends =
                  HS.union dep (HS.fromList (snd <$> drvDependent drv))
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
                              DrvExt e -> mkSelect (mkSym (extFrom e)) (extPath e)
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
            (Fix $ NSet Recursive (toNix <$> HS.toList builds))
            [drvId drv]
     in mkNixMod
          ( if HS.null extern
              then body
              else mkAbs (HS.toList extern) body
          )
    where
      toNix d = mkBind (drvId d) (buildDrvArg (drvInfo d))
  build (DrvExt (ExtDep {extFrom = f, extPath = i})) =
    mkNixMod (mkAbs [f] (mkSelect (mkSym f) i))