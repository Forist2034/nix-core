module Derivation.InputAddressedSpec (
  simple,
  multiOutput,
  dependOnSrc,
  dependOnIA,
  spec,
) where

import Derivation.MultiOutput
import Derivation.Test
import HsNix.Derivation.Backend
import HsNix.Derivation.DerivationArgs
import HsNix.Hash
import SrcInputSpec hiding (spec)
import Test.Hspec

simple :: DrvTest
simple =
  DrvTest
    { dtName = "simple derivation",
      dtDerivation =
        derivation @SHA256
          ( (simpleDrvArg "simple-ia" "builder")
              { drvArgs = ["1234567", "7654321"],
                drvEnv = [("E1", "environment 1"), ("E2", "environment 2")],
                drvPassAsFile = [("PassFile1", "12345"), ("PassFile2", "54321")]
              }
          )
          []
          [],
      dtHashType = RegularHash
    }

multiOutput :: DrvTest
multiOutput =
  DrvTest
    { dtName = "multi output",
      dtDerivation =
        derivation
          @SHA256
          ( (simpleDrvArg "multi-out-ia" "builder")
              { drvType = InputAddressed outputList
              }
          )
          []
          [],
      dtHashType = RegularHash
    }

dependOnSrc :: DrvTest
dependOnSrc =
  DrvTest
    { dtName = "depend on source",
      dtDerivation =
        derivationWithDep @SHA256
          (simpleDrvArg "dep-src-ia" "builder")
          (InputAddressed outputList)
          [("source", textSrc.stInput)]
          [],
      dtHashType = RegularHash
    }

dependOnIA :: DrvTest
dependOnIA =
  DrvTest
    { dtName = "depend on input addressed drv",
      dtDerivation =
        derivationWithDep @SHA256
          (simpleDrvArg "dep-ia-ia" "builder")
          (InputAddressed outputList)
          []
          [ ("inputIA1", simple.dtDerivation),
            ("inputIA2", dependOnSrc.dtDerivation)
          ],
      dtHashType = RegularHash
    }

spec :: Spec
spec =
  runDrvTests
    [ simple,
      multiOutput,
      dependOnSrc,
      dependOnIA
    ]