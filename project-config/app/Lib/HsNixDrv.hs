module Lib.HsNixDrv (hsnixDrv) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as S
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.PackageDescription
import Distribution.Version
import HsCabal
import HsCabal.QQ
import Language.Haskell.Extension
import System.Directory
import System.FilePath
import Utils

hsnixDrv :: IO LocalPackage
hsnixDrv =
  let name = "hsnix-drv"
      root = "hsnix-drv"
      commonDep =
        [ "base" @@ [versionRangeQ| >= 4.15 && < 4.18 |],
          "text" ^>= [2, 0]
        ]
      hsnixCore =
        Dependency
          "hsnix-core"
          anyVersion
          ( NES.fromNonEmpty
              ( LSubLibName "core-types"
                  :| [LSubLibName "derivation-types"]
              )
          )
   in withCurrentDirectory root $ do
        storepath <-
          addLibraryMod
            ( emptyLibrary
                { libName = LSubLibName "storepath",
                  libBuildInfo =
                    (simpleBuildInfo "storepath" mempty)
                      { targetBuildDepends =
                          commonDep
                            ++ [anyVersionDep "hashable"]
                      }
                }
            )
        lib <-
          addLibraryMod
            ( emptyLibrary
                { reexportedModules =
                    fmap
                      simpleReexport
                      (exposedModules storepath),
                  libBuildInfo =
                    (simpleBuildInfo "src" mempty)
                      { defaultExtensions =
                          [ EnableExtension OverloadedStrings,
                            EnableExtension Strict
                          ],
                        otherModules = ["HsNix.DrvDirect.Internal.Hash"],
                        targetBuildDepends =
                          commonDep
                            ++ [ "bytestring" ^>= [0, 11],
                                 "containers" ^>= [0, 6, 6],
                                 "unordered-containers" ^>= [0, 2],
                                 "vector" ^>= [0, 13],
                                 "hashable" ^>= [1, 4, 2],
                                 "memory" @@ [versionRangeQ| >= 0.16 && < 0.19 |],
                                 "binary" ^>= [0, 8],
                                 "cryptonite" @@ [versionRangeQ| >= 0.29 && < 0.31 |],
                                 "nix-derivation" ^>= [1, 1],
                                 anyVersionDep "nix-archive",
                                 "hnix-store-core" ^>= [0, 6],
                                 "hnix-store-remote" ^>= [0, 6],
                                 hsnixCore,
                                 Dependency
                                  name
                                  anyVersion
                                  (NES.singleton (libName storepath))
                               ]
                      }
                }
            )
        testData <-
          fmap (\p -> "tests" </> "spec" </> "files" </> p) . S.toAscList
            <$> listDirRec ("tests" </> "spec" </> "files")
        testSpec <- do
          mods <- listMod "hs" ("tests" </> "spec")
          pure
            ( emptyTestSuite
                { testName = "spec",
                  testInterface = TestSuiteExeV10 (mkVersion [1, 0]) "Main.hs",
                  testBuildInfo =
                    (simpleBuildInfo ("tests" </> "spec") mempty)
                      { otherModules = S.toAscList (S.delete "Main" mods),
                        defaultExtensions =
                          [ EnableExtension OverloadedStrings,
                            EnableExtension OverloadedRecordDot,
                            EnableExtension TypeApplications,
                            EnableExtension StrictData
                          ],
                        buildToolDepends =
                          [ ExeDependency
                              "hspec-discover"
                              "hspec-discover"
                              (majorBoundVersion (mkVersion [2]))
                          ],
                        targetBuildDepends =
                          commonDep
                            ++ [ "hspec" @@ [versionRangeQ| >= 2.8 && < 2.11 |],
                                 "aeson" ^>= [2, 1],
                                 anyVersionDep "bytestring",
                                 anyVersionDep "containers",
                                 anyVersionDep "unordered-containers",
                                 anyVersionDep "filepath",
                                 anyVersionDep "cryptonite",
                                 anyVersionDep "nix-archive",
                                 anyVersionDep "hnix-store-core",
                                 Dependency name anyVersion mainLibSet,
                                 hsnixCore
                               ]
                      }
                }
            )
        let testSignature =
              emptyTestSuite
                { testName = "signature",
                  testInterface = TestSuiteExeV10 (mkVersion [1, 0]) "Main.hs",
                  testBuildInfo =
                    (simpleBuildInfo ("tests" </> "signature") mempty)
                      { targetBuildDepends =
                          [ anyVersionDep "base",
                            Dependency name anyVersion mainLibSet,
                            anyVersionDep "hsnix-core"
                          ]
                      }
                }
        pure
          ( LocalPackage
              { lpName = name,
                lpRoot = root,
                lpDesc =
                  emptyGenericPackageDescription
                    { packageDescription =
                        (simplePkgDesc name [0, 1, 0, 0])
                          { extraSrcFiles = testData
                          },
                      condLibrary = Just (unConditional lib),
                      condSubLibraries = [unConditionalSubLib storepath],
                      condTestSuites =
                        [ unConditionalTest testSpec,
                          unConditionalTest testSignature
                        ]
                    }
              }
          )