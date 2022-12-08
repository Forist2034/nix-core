{-# LANGUAGE DeriveGeneric #-}

module Nix.Internal.Derivation (DerivationArg (..), defaultDrvArg) where

import Data.Default
import Data.Hashable (Hashable)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import GHC.Generics (Generic)
import Nix.Hash
import Nix.Internal.System

data DerivationArg txt d = DerivationArg
  { drvName :: Text,
    drvBuilder :: txt,
    drvSystem :: System,
    drvArgs :: [txt],
    drvEnv, drvPassAsFile :: [(Text, txt)],
    drvOutputs :: NEL.NonEmpty Text,
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
      drvOutputs = NEL.singleton "out",
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
