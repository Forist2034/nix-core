{-# LANGUAGE TemplateHaskell #-}

module Derivation.Test (
  DrvHashType (..),
  DrvTest (..),
  runDrvTest,
  runDrvTests,
  simpleHash,
  simpleFixedOutput,
  simpleDrvArg,
  derivationWithDep,
) where

import Crypto.Hash
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import HsNix.Derivation.Backend
import HsNix.Derivation.Types as DT hiding (DerivationArg)
import HsNix.DrvDirect.Internal.Types
import HsNix.DrvStr
import HsNix.Hash
import HsNix.Internal.OutputName
import HsNix.Internal.StorePathName
import HsNix.System
import System.FilePath
import qualified System.Nix.StorePath as NSP
import Test.Hspec

data DrvInfo = DrvInfo
  { diName :: Text,
    diDrvPath :: Text,
    diRefs :: HS.HashSet Text,
    diDefaultOutput :: Text,
    diDefaultStorePath :: Text,
    diOutputs :: HM.HashMap Text Text
  }
  deriving (Show, Eq)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '-' . drop 2}
  ''DrvInfo

data DrvTest = DrvTest
  { dtName :: String,
    dtDerivation :: Derivation,
    dtHashType :: DrvHashType
  }

runDrvTest :: DrvTest -> Spec
runDrvTest dt =
  describe dt.dtName $ do
    let drv = dt.dtDerivation
        name = T.unpack drv.drvName
    it "derivation content" $ do
      expected <- BS.readFile (testDataDir </> name <.> "drv")
      TE.encodeUtf8 drv.drvText `shouldBe` expected
    it "hash type" (drv.drvHashType `shouldBe` dt.dtHashType)
    it "derivation info" $ do
      expected <- decodeFileStrict (testDataDir </> name <.> "json")
      DrvInfo
        { diName = drv.drvName,
          diDrvPath = drv.drvPathText,
          diRefs = HS.map NSP.storePathToText drv.drvRefs,
          diDefaultOutput = outputNameText drv.drvDefaultOut,
          diDefaultStorePath = drv.drvDefaultSPText,
          diOutputs = HM.mapKeys outputNameText drv.drvOutput
        }
        `shouldBe` fromJust expected
  where
    testDataDir = "tests" </> "spec" </> "files"

runDrvTests :: [DrvTest] -> Spec
runDrvTests = traverse_ runDrvTest

simpleHash :: Hash SHA256
simpleHash = Hash (hashWith @ByteString SHA256 "1234567")

simpleFixedOutput :: HashMode -> DerivType SHA256
simpleFixedOutput t = defaultFixedOutput t simpleHash

addDrvDep :: Text -> Derivation -> DerivationArg a -> DerivationArg a
addDrvDep prefix d da =
  da
    { drvArgs = DStr d.drvDefaultSPText : da.drvArgs,
      drvEnv =
        fmap
          (\(OutputName o, p) -> (prefix <> o, DStr p))
          (HM.toList d.drvOutput)
          ++ da.drvEnv
    }

simpleDrvArg :: Text -> Text -> DerivationArg a
simpleDrvArg n b = defaultDrvArg (StorePathName n) (DStr b) x86_64_linux

withDrvDep :: [(Text, Derivation)] -> DerivationArg a -> DerivationArg a
withDrvDep ds ida = foldr' (\(p, d) da -> addDrvDep p d da) ida ds

addSrcDep :: Text -> SrcInput -> DerivationArg a -> DerivationArg a
addSrcDep n si da =
  da
    { drvPassAsFile = (n, DStr si.srcSPText) : da.drvPassAsFile
    }

withSrcDep :: [(Text, SrcInput)] -> DerivationArg a -> DerivationArg a
withSrcDep ss ida = foldr' (\(p, s) da -> addSrcDep p s da) ida ss

asInputDrv :: Derivation -> (Derivation, [Maybe OutputName])
asInputDrv d = (d, Nothing : (Just <$> HM.keys d.drvOutput))

derivationWithDep ::
  NamedHashAlgo a =>
  DerivationArg a ->
  DerivType a ->
  [(Text, SrcInput)] ->
  [(Text, Derivation)] ->
  Derivation
derivationWithDep d dt si di =
  derivation
    (withSrcDep si (withDrvDep di (d {drvType = dt})))
    (fmap snd si)
    (fmap (asInputDrv . snd) di)
