module HsNix.DrvDirect.Internal.Types (
  SrcData (..),
  SrcInput (..),
  DrvHashType (..),
  DerivationArg,
  Derivation (..),
  BuildResult (..),
) where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Text (Text)
import qualified HsNix.Derivation.DerivationArgs as DT
import HsNix.DrvStr
import qualified HsNix.Internal.OutputName as ON
import qualified HsNix.StorePath as SP
import Nix.Nar (Nar)
import System.Nix.StorePath

data SrcData
  = SrcText Text
  | SrcBinary ByteString
  | SrcNar Nar
  deriving (Show)

data SrcInput = SrcInput
  { srcName :: Text,
    srcData :: SrcData,
    srcSP :: StorePath,
    srcSPText :: Text
  }
  deriving (Show)

instance Eq SrcInput where
  l == r = srcSPText l == srcSPText r

instance Hashable SrcInput where
  hashWithSalt s = hashWithSalt s . srcSPText

data DrvHashType
  = RegularHash
  | DeferredHash
  deriving (Show, Eq)

type DerivationArg = DT.DerivationArg DrvStr SP.StorePath

data Derivation = Derivation
  { drvName :: Text,
    drvText :: Text,
    drvPath :: StorePath,
    drvPathText :: Text,
    drvHash :: Text,
    drvHashType :: DrvHashType,
    drvRefs :: StorePathSet,
    drvDefaultOut :: ON.OutputName,
    drvDefaultSPText :: Text,
    drvOutput :: HM.HashMap ON.OutputName Text
  }
  deriving (Show)

instance Eq Derivation where
  l == r = drvPath l == drvPath r

instance Hashable Derivation where
  hashWithSalt s = hashWithSalt s . drvPath

data BuildResult = BuildResult
  { brSource :: [SrcInput],
    brDrv :: [Derivation]
  }