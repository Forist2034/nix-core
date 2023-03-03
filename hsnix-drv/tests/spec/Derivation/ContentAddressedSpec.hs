module Derivation.ContentAddressedSpec (
  flatHash,
  recHash,
  multiOutput,
  dependOnSrc,
  dependOnCA,
  spec,
) where

import qualified Data.List.NonEmpty as NEL
import Derivation.MultiOutput
import Derivation.Test
import HsNix.Derivation.Backend
import HsNix.Derivation.Types
import HsNix.Hash
import HsNix.Internal.OutputName
import SrcInputSpec hiding (spec)
import Test.Hspec

flatHash :: DrvTest
flatHash =
  DrvTest
    { dtName = "hash flat",
      dtDerivation =
        derivation @SHA256
          ( (simpleDrvArg "simple-ca" "builder")
              { drvArgs = ["1234567", "7654321"],
                drvEnv = [("E1", "env1"), ("E2", "env2")],
                drvPassAsFile = [("PF1", "file1"), ("PF2", "file2")],
                drvType =
                  ContentAddressed
                    HashFlat
                    (NEL.singleton (OutputName "out"))
              }
          )
          []
          [],
      dtHashType = DeferredHash
    }

recHash :: DrvTest
recHash =
  DrvTest
    { dtName = "hash recursive",
      dtDerivation =
        derivation @SHA256
          ( (simpleDrvArg "simple-ca-rec" "builder")
              { drvArgs = ["1234567", "7654321"],
                drvEnv =
                  [ ("E1", "environment1"),
                    ("E2", "environment2")
                  ],
                drvPassAsFile = [("FILE", "1223\n3211\n")],
                drvType =
                  ContentAddressed
                    HashRecursive
                    (NEL.singleton (OutputName "out"))
              }
          )
          []
          [],
      dtHashType = DeferredHash
    }

multiOutput :: DrvTest
multiOutput =
  DrvTest
    { dtName = "multi output",
      dtDerivation =
        derivation @SHA256
          ( (simpleDrvArg "multi-out-ca" "builder")
              { drvType = ContentAddressed HashRecursive outputList
              }
          )
          []
          [],
      dtHashType = DeferredHash
    }

dependOnSrc :: DrvTest
dependOnSrc =
  DrvTest
    { dtName = "depend on source",
      dtDerivation =
        derivationWithDep @SHA256
          (simpleDrvArg "dep-src-ca" "builder")
          (ContentAddressed HashRecursive outputList)
          [("source", textSrc.stInput)]
          [],
      dtHashType = DeferredHash
    }

dependOnCA :: DrvTest
dependOnCA =
  let depSrcDrv = dependOnSrc.dtDerivation
   in DrvTest
        { dtName = "depend on content addressed",
          dtDerivation =
            derivationWithDep @SHA256
              (simpleDrvArg "dep-ca-ca" "builder")
              (ContentAddressed HashRecursive outputList)
              []
              [ ("inputCA1", flatHash.dtDerivation),
                ("inputCA2", depSrcDrv)
              ],
          dtHashType = DeferredHash
        }

spec :: Spec
spec =
  runDrvTests
    [ flatHash,
      recHash,
      multiOutput,
      dependOnSrc,
      dependOnCA
    ]
