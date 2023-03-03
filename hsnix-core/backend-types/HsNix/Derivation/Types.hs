{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module HsNix.Derivation.Types (
  StorePathName,
  validStorePathName,
  makeStorePathName,
  makeStorePathNameOrFail,
  OutputName,
  validOutputName,
  makeOutputName,
  makeOutputNameOrFail,
  DerivType (..),
  Reference (..),
  DerivationArg (..),
  defaultDrvArg,
  FetchUrlArg (..),
  defFetchUrlArg,
) where

import Data.Hashable (Hashable)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import GHC.Generics
import HsNix.DrvStr
import HsNix.Hash
import HsNix.Internal.OutputName
import HsNix.Internal.StorePathName
import HsNix.StorePath
import HsNix.System

data DerivType a
  = InputAddressed (NEL.NonEmpty OutputName)
  | FixedOutput
      { drvImpureEnvs :: [Text],
        drvHashMode :: HashMode,
        drvHash :: Hash a
      }
  | ContentAddressed HashMode (NEL.NonEmpty OutputName)
  deriving (Show, Eq, Generic)

instance Hashable (DerivType a)

data Reference
  = RefSelf OutputName
  | RefDrv StorePath
  deriving (Show, Eq, Generic)

instance Hashable Reference

data DerivationArg a = DerivationArg
  { drvName :: StorePathName,
    drvBuilder :: DrvStr,
    drvSystem :: System,
    drvArgs :: [DrvStr],
    drvEnv, drvPassAsFile :: [(Text, DrvStr)],
    drvType :: DerivType a,
    drvExportReferencesGraph :: [(Text, StorePath)],
    drvAllowedReferences,
    drvAllowedRequisites,
    drvDisallowedReferences,
    drvDisallowedRequisites ::
      Maybe [Reference],
    drvPreferLocalBuild, drvAllowSubstitutes :: Bool
  }
  deriving (Show, Eq, Generic)

instance Hashable (DerivationArg a)

defaultDrvArg :: StorePathName -> DrvStr -> System -> DerivationArg a
defaultDrvArg n b s =
  DerivationArg
    { drvName = n,
      drvBuilder = b,
      drvSystem = s,
      drvArgs = [],
      drvEnv = [],
      drvPassAsFile = [],
      drvType = InputAddressed (NEL.singleton (OutputName "out")),
      drvExportReferencesGraph = [],
      drvAllowedReferences = Nothing,
      drvAllowedRequisites = Nothing,
      drvDisallowedReferences = Nothing,
      drvDisallowedRequisites = Nothing,
      drvPreferLocalBuild = False,
      drvAllowSubstitutes = True
    }

data FetchUrlArg a = FetchUrlArg
  { faName :: StorePathName,
    faUrl :: Text,
    faOutputHash :: Hash a,
    faIsExecutable, faUnpack :: Bool
  }
  deriving (Eq, Show)

defFetchUrlArg :: StorePathName -> Text -> Hash a -> FetchUrlArg a
defFetchUrlArg n u h =
  FetchUrlArg
    { faName = n,
      faUrl = u,
      faOutputHash = h,
      faIsExecutable = False,
      faUnpack = False
    }