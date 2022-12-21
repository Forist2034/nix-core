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

data DerivOutput a
  = RegularOutput (NEL.NonEmpty Text)
  | FixedOutput (Hash a)
  deriving (Show, Eq, Generic)

instance Hashable (DerivOutput a)

data DerivationArg txt a = DerivationArg
  { drvName :: Text,
    drvBuilder :: txt,
    drvSystem :: System,
    drvArgs :: [txt],
    drvEnv, drvPassAsFile :: [(Text, txt)],
    drvOutputs :: DerivOutput a,
    drvHashMode :: HashMode,
    drvAllowedReferences,
    drvAllowedRequisites,
    drvDisallowedReferences,
    drvDisallowedRequisites ::
      Maybe [txt],
    drvPreferLocalBuild, drvAllowSubstitutes :: Bool
  }
  deriving (Show, Eq, Generic)

instance (Hashable txt) => Hashable (DerivationArg txt a)

defaultDrvArg :: Text -> txt -> System -> DerivationArg txt a
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
      drvAllowedReferences = Nothing,
      drvAllowedRequisites = Nothing,
      drvDisallowedReferences = Nothing,
      drvDisallowedRequisites = Nothing,
      drvPreferLocalBuild = False,
      drvAllowSubstitutes = True
    }
