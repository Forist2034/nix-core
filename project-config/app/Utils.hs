module Utils (
  simplePkgDesc,
  unConditionalTest,
  unConditionalSubLib,
  simpleReexport,
) where

import Data.Maybe
import Distribution.CabalSpecVersion
import Distribution.License
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.Utils.Path
import Distribution.Version
import HsCabal

simplePkgDesc :: PackageName -> [Int] -> PackageDescription
simplePkgDesc n v =
  emptyPackageDescription
    { specVersion = CabalSpecV3_4,
      package = PackageIdentifier n (mkVersion v),
      licenseRaw = Right MIT,
      licenseFiles = [unsafeMakeSymbolicPath "LICENSE"],
      maintainer = "dariankline@outlook.com",
      author = "Jose Jane",
      buildTypeRaw = Just Simple
    }

unConditionalTest :: TestSuite -> (UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)
unConditionalTest t = (testName t, unConditional t)

simpleReexport :: ModuleName -> ModuleReexport
simpleReexport m =
  ModuleReexport
    { moduleReexportOriginalPackage = Nothing,
      moduleReexportOriginalName = m,
      moduleReexportName = m
    }

unConditionalSubLib ::
  Library ->
  (UnqualComponentName, CondTree ConfVar [Dependency] Library)
unConditionalSubLib l =
  ( fromJust (libraryNameString (libName l)),
    unConditional l
  )