{-# LANGUAGE DeriveGeneric #-}

module HsNix.Internal.Derivation
  ( DerivOutput (..),
    DerivationArg (..),
    defaultDrvArg,
  )
where

import Data.Hashable (Hashable)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import GHC.Generics (Generic)
import HsNix.Hash
import HsNix.System

data DerivOutput
  = RegularOutput (NEL.NonEmpty Text)
  | FixedOutput Hash
  deriving (Show, Eq, Generic)

instance Hashable DerivOutput

data DerivationArg txt = DerivationArg
  { drvName :: Text,
    drvBuilder :: txt,
    drvSystem :: System,
    drvArgs :: [txt],
    drvEnv, drvPassAsFile :: [(Text, txt)],
    drvOutputs :: DerivOutput,
    drvHashMode :: HashMode,
    drvHashAlgo :: HashAlgo,
    drvAllowedReferences,
    drvAllowedRequisites,
    drvDisallowedReferences,
    drvDisallowedRequisites ::
      Maybe [txt],
    drvPreferLocalBuild, drvAllowSubstitutes :: Bool
  }
  deriving (Show, Eq, Generic)

instance (Hashable txt) => Hashable (DerivationArg txt)

defaultDrvArg :: Text -> txt -> System -> DerivationArg txt
defaultDrvArg n b s =
  DerivationArg
    { drvName = n,
      drvBuilder = b,
      drvSystem = s,
      drvArgs = [],
      drvEnv = [],
      drvPassAsFile = [],
      drvOutputs = RegularOutput (NEL.singleton "out"),
      drvHashMode = HashRecursive,
      drvHashAlgo = HashSha256,
      drvAllowedReferences = Nothing,
      drvAllowedRequisites = Nothing,
      drvDisallowedReferences = Nothing,
      drvDisallowedRequisites = Nothing,
      drvPreferLocalBuild = False,
      drvAllowSubstitutes = True
    }
