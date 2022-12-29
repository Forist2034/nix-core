{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}

module HsNix.Internal.Derivation
  ( DerivType (..),
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

data DerivType a
  = InputAddressed (NEL.NonEmpty Text)
  | FixedOutput HashMode (Hash a)
  deriving (Show, Eq, Generic)

instance Hashable (DerivType a)

data DerivationArg txt a = DerivationArg
  { drvName :: Text,
    drvBuilder :: txt,
    drvSystem :: System,
    drvArgs :: [txt],
    drvEnv, drvPassAsFile :: [(Text, txt)],
    drvType :: DerivType a,
    drvAllowedReferences,
    drvAllowedRequisites,
    drvDisallowedReferences,
    drvDisallowedRequisites ::
      Maybe [txt],
    drvPreferLocalBuild, drvAllowSubstitutes :: Bool
  }
  deriving (Show, Eq, Generic)

instance (Hashable txt) => Hashable (DerivationArg txt a)

defaultDrvArg :: forall a txt. Text -> txt -> System -> DerivationArg txt a
defaultDrvArg n b s =
  DerivationArg
    { drvName = n,
      drvBuilder = b,
      drvSystem = s,
      drvArgs = [],
      drvEnv = [],
      drvPassAsFile = [],
      drvType = InputAddressed (NEL.singleton "out"),
      drvAllowedReferences = Nothing,
      drvAllowedRequisites = Nothing,
      drvDisallowedReferences = Nothing,
      drvDisallowedRequisites = Nothing,
      drvPreferLocalBuild = False,
      drvAllowSubstitutes = True
    }
