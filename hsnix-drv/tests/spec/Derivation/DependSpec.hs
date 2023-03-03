module Derivation.DependSpec (spec) where

import Derivation.MultiOutput
import Derivation.Test
import HsNix.Derivation.Backend
import HsNix.Derivation.Types
import HsNix.Hash
import Test.Hspec

simpleIA :: Derivation
simpleIA =
  derivation @SHA256
    ( (simpleDrvArg "simple-ia" "builder")
        { drvType = InputAddressed outputList
        }
    )
    []
    []

simpleFOD :: Derivation
simpleFOD =
  derivation
    ( (simpleDrvArg "simple-fod" "builder")
        { drvType =
            FixedOutput
              { drvImpureEnvs = [],
                drvHashMode = HashRecursive,
                drvHash = simpleHash
              }
        }
    )
    []
    []

simpleCA :: Derivation
simpleCA =
  derivation
    @SHA256
    ( (simpleDrvArg "simple-ca" "builder")
        { drvType = ContentAddressed HashRecursive outputList
        }
    )
    []
    []

iaDependFOD :: DrvTest
iaDependFOD =
  DrvTest
    { dtName = "depend on fixed output",
      dtDerivation =
        derivationWithDep @SHA256
          (simpleDrvArg "dep-fod-ia" "builder")
          (InputAddressed outputList)
          []
          [("FOD", simpleFOD)],
      dtHashType = RegularHash
    }

iaDependCA :: DrvTest
iaDependCA =
  DrvTest
    { dtName = "depend on contend addressed",
      dtDerivation =
        derivationWithDep @SHA256
          (simpleDrvArg "dep-ca-ia" "builder")
          (InputAddressed outputList)
          []
          [("CA", simpleCA)],
      dtHashType = DeferredHash
    }

iaDependDeferIA :: DrvTest
iaDependDeferIA =
  DrvTest
    { dtName = "depend on deferred input addressed",
      dtDerivation =
        derivationWithDep @SHA256
          (simpleDrvArg "dep-deferred-ia-ia" "builder")
          (InputAddressed outputList)
          []
          [ ("simpleIA", simpleIA),
            ("deferredIA", iaDependCA.dtDerivation)
          ],
      dtHashType = DeferredHash
    }

caDependFOD :: DrvTest
caDependFOD =
  DrvTest
    { dtName = "depend on fixed output",
      dtDerivation =
        derivationWithDep @SHA256
          (simpleDrvArg "dep-fod-ca" "builder")
          (ContentAddressed HashRecursive outputList)
          []
          [("FOD", simpleFOD)],
      dtHashType = DeferredHash
    }

caDependRegularIA :: DrvTest
caDependRegularIA =
  DrvTest
    { dtName = "depend on regular input addressed",
      dtDerivation =
        derivationWithDep @SHA256
          (simpleDrvArg "dep-regular-ia-ca" "builder")
          (ContentAddressed HashRecursive outputList)
          []
          [("regularIA", simpleIA)],
      dtHashType = DeferredHash
    }

caDependIA :: DrvTest
caDependIA =
  DrvTest
    { dtName = "depend on input addressed",
      dtDerivation =
        derivationWithDep @SHA256
          (simpleDrvArg "dep-ia-ca" "builder")
          (ContentAddressed HashRecursive outputList)
          []
          [ ("regularIA", simpleIA),
            ("deferredIA", iaDependDeferIA.dtDerivation)
          ],
      dtHashType = DeferredHash
    }

spec :: Spec
spec = do
  describe "input addressed" $
    runDrvTests [iaDependFOD, iaDependCA, iaDependDeferIA]
  describe "content addressed" $
    runDrvTests [caDependFOD, caDependIA, caDependRegularIA]
