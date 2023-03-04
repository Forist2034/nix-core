module Lib.NixArchive (nixArchive) where

import qualified Data.Set as S
import Distribution.PackageDescription
import Distribution.Version
import HsCabal
import HsCabal.QQ
import Language.Haskell.Extension
import System.Directory
import System.FilePath
import Utils

nixArchive :: IO LocalPackage
nixArchive =
  let name = "nix-archive"
      root = "nix-archive"
      deps =
        [ "base" @@ [versionRangeQ| >= 4.15 && < 4.18 |],
          "filepath" ^>= [1, 4, 100],
          "bytestring" ^>= [0, 11, 2],
          "containers" ^>= [0, 6, 6],
          "binary" ^>= [0, 8]
        ]
      exts = [EnableExtension OverloadedStrings]
   in withCurrentDirectory root $ do
        lib <-
          addLibraryMod
            ( emptyLibrary
                { libBuildInfo =
                    (simpleBuildInfo "src" mempty)
                      { defaultExtensions = exts,
                        targetBuildDepends =
                          deps
                            ++ [ "directory" ^>= [1, 3, 8],
                                 "unix" ^>= [2, 8],
                                 "hashable" ^>= [1, 4, 2],
                                 anyVersionDep "template-haskell"
                               ]
                      }
                }
            )
        testData <-
          fmap (\p -> "tests" </> "spec" </> "files" </> p)
            . S.toAscList
            <$> listDirRec ("tests" </> "spec" </> "files")
        let testSpec =
              emptyTestSuite
                { testName = "spec",
                  testInterface = TestSuiteExeV10 (mkVersion [1, 0]) "Main.hs",
                  testBuildInfo =
                    (simpleBuildInfo ("tests" </> "spec") mempty)
                      { defaultExtensions = exts,
                        targetBuildDepends =
                          deps
                            ++ [ "hspec" @@ [versionRangeQ| >=2.8 && < 2.11|],
                                 anyVersionDep name
                               ]
                      }
                }
        pure
          ( LocalPackage
              { lpName = name,
                lpRoot = "nix-archive",
                lpDesc =
                  emptyGenericPackageDescription
                    { packageDescription =
                        (simplePkgDesc name [0, 1, 0, 0])
                          { extraSrcFiles = testData
                          },
                      condLibrary = Just (unConditional lib),
                      condTestSuites = [unConditionalTest testSpec]
                    }
              }
          )