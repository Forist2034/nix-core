{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module HsNix.Derivation.DerivationArgs (
  DerivType (..),
  defaultFixedOutput,
  Reference (..),
  DerivationArg (..),
  defaultDrvArg,
) where

import Data.Hashable (Hashable)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import GHC.Generics
import HsNix.Hash
import HsNix.Internal.OutputName
import HsNix.StorePathName
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

defaultFixedOutput :: HashMode -> Hash a -> DerivType a
defaultFixedOutput hm h =
  FixedOutput
    { drvImpureEnvs = [],
      drvHashMode = hm,
      drvHash = h
    }

data Reference sp
  = RefSelf OutputName
  | RefDrv sp
  deriving (Show, Eq, Generic)

instance Hashable sp => Hashable (Reference sp)

{-
  Depencency like signature -> module -> signature seems to cause ghc
   interface file error. Avoid that by passing StorePath and DrvStr as
   type arg to DerivationArg.
-}

data DerivationArg txt sp a = DerivationArg
  { drvName :: StorePathName,
    drvBuilder :: txt,
    drvSystem :: System,
    drvArgs :: [txt],
    drvEnv, drvPassAsFile :: [(Text, txt)],
    drvType :: DerivType a,
    drvExportReferencesGraph :: [(Text, sp)],
    drvAllowedReferences,
    drvAllowedRequisites,
    drvDisallowedReferences,
    drvDisallowedRequisites ::
      Maybe [Reference sp],
    drvPreferLocalBuild, drvAllowSubstitutes :: Bool
  }
  deriving (Show, Eq, Generic)

instance (Hashable txt, Hashable sp) => Hashable (DerivationArg txt sp a)

defaultDrvArg :: StorePathName -> txt -> System -> DerivationArg txt sp a
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
