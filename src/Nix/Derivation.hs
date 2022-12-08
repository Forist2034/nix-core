{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Derivation
  ( DerivationArg (..),
    defaultDrvArg,
    IsDrvStr (..),
    (</>),
    MonadDeriv (..),
  )
where

import Data.Default (Default (def))
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Nix.Hash
import Nix.Internal.System (System)

data DerivationArg txt d = DerivationArg
  { drvName :: Text,
    drvBuilder :: txt,
    drvSystem :: System,
    drvArgs :: [txt],
    drvEnv, drvPassAsFile :: [(Text, txt)],
    drvOutputs :: [Text],
    drvHashMode :: HashMode,
    drvHashAlgo :: HashAlgo,
    drvHash :: Maybe Hash,
    drvAllowedReferences,
    drvAllowedRequisites,
    drvDisallowedReferences,
    drvDisallowedRequisites ::
      Maybe [d],
    drvPreferLocalBuild, drvAllowSubstitutes :: Bool
  }
  deriving (Show, Eq, Generic)

instance (Hashable txt, Hashable d) => Hashable (DerivationArg txt d)

defaultDrvArg :: Text -> txt -> System -> DerivationArg txt d
defaultDrvArg n b s =
  DerivationArg
    { drvName = n,
      drvBuilder = b,
      drvSystem = s,
      drvArgs = [],
      drvEnv = [],
      drvPassAsFile = [],
      drvOutputs = ["out"],
      drvHashMode = def,
      drvHashAlgo = def,
      drvHash = Nothing,
      drvAllowedReferences = Nothing,
      drvAllowedRequisites = Nothing,
      drvDisallowedReferences = Nothing,
      drvDisallowedRequisites = Nothing,
      drvPreferLocalBuild = False,
      drvAllowSubstitutes = True
    }

type DrvValue m = (Eq m, Hashable m, Show m)

class (IsString m, Monoid m, DrvValue m) => IsDrvStr m where
  fromText :: Text -> m

(</>) :: (IsDrvStr m) => m -> m -> m
l </> r = l <> "/" <> r

class
  (Monad m, IsDrvStr (DrvStr m), DrvValue (StorePath m), DrvValue (Derivation m)) =>
  MonadDeriv m
  where
  type DrvStr m
  data StorePath m
  data Derivation m
  data BuildResult m
  derivation :: m (DerivationArg (DrvStr m) (Derivation m)) -> Derivation m
  addFile :: Text -> Text -> StorePath m
  pathToStr :: StorePath m -> DrvStr m
  storePathOf :: Derivation m -> Maybe Text -> m (StorePath m)
  build :: Derivation m -> BuildResult m