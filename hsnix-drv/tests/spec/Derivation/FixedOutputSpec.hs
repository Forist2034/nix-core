module Derivation.FixedOutputSpec (
  hashFlat,
  hashRec,
  dependOnSrc,
  dependOnFOD,
  spec,
) where

import Derivation.Test
import HsNix.Derivation.Backend
import HsNix.Derivation.Types
import HsNix.Hash
import SrcInputSpec hiding (spec)
import Test.Hspec

hashFlat :: DrvTest
hashFlat =
  DrvTest
    { dtName = "hash flat",
      dtDerivation =
        derivation
          ( (simpleDrvArg "simple-fod1" "builder")
              { drvArgs = ["12345", "67890"],
                drvEnv = [("ENV1", "v1"), ("ENV2", "v2")],
                drvPassAsFile = [("FILE1", "content1"), ("FILE2", "content2")],
                drvType = simpleFixedOutput HashFlat
              }
          )
          []
          [],
      dtHashType = RegularHash
    }

hashRec :: DrvTest
hashRec =
  DrvTest
    { dtName = "hash recursive",
      dtDerivation =
        derivation
          ( (simpleDrvArg "simple-fod-rec" "builder")
              { drvArgs = ["123", "456"],
                drvEnv = [("E1", "v1"), ("E2", "v2")],
                drvPassAsFile = [("F1", "c1"), ("F2", "c2")],
                drvType = simpleFixedOutput HashRecursive
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
        derivationWithDep
          (simpleDrvArg "dep-src-fod" "builder")
          (simpleFixedOutput HashRecursive)
          [("source", textSrc.stInput)]
          [],
      dtHashType = RegularHash
    }

dependOnFOD :: DrvTest
dependOnFOD =
  DrvTest
    { dtName = "depend on FOD",
      dtDerivation =
        derivationWithDep
          (simpleDrvArg "dep-fod-fod" "builder")
          (simpleFixedOutput HashRecursive)
          []
          [ ("inputFOD1", hashRec.dtDerivation),
            ("inputFOD2", dependOnSrc.dtDerivation)
          ],
      dtHashType = RegularHash
    }

spec :: Spec
spec =
  runDrvTests
    [ hashFlat,
      hashRec,
      dependOnSrc,
      dependOnFOD
    ]