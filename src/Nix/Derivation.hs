{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Derivation
  ( DerivationArg (..),
    defaultDrvArg,
    MonadDeriv (..),
  )
where

import Data.Default (Default (def))
import Data.Hashable (Hashable)
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
    drvDependent :: [(Text, d)],
    drvOutputs :: [Text],
    drvHashMode :: HashMode,
    drvHashAlgo :: HashAlgo,
    drvHash :: Maybe Hash,
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
      drvDependent = [],
      drvOutputs = ["out"],
      drvHashMode = def,
      drvHashAlgo = def,
      drvHash = Nothing,
      drvPreferLocalBuild = False,
      drvAllowSubstitutes = True
    }

class (Monad m) => MonadDeriv m where
  type DrvStr m
  data StorePath m
  data Derivation m
  data BuildResult m
  derivation :: m (DerivationArg (DrvStr m) (Derivation m)) -> Derivation m
  addFile :: Text -> Text -> StorePath m
  pathToStr :: StorePath m -> DrvStr m
  storePath :: Derivation m -> Text -> m (StorePath m)
  build :: Derivation m -> BuildResult m