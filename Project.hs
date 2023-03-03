{- cabal:
    build-depends: base, Cabal, Cabal-syntax, Cabal-hs, cabal-install, filepath, directory, containers
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Foldable
import Data.Maybe
import Data.Set qualified as S
import Distribution.CabalSpecVersion
import Distribution.Client.ProjectConfig
import Distribution.Compat.NonEmptySet qualified as NES
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName (ModuleName, components)
import Distribution.PackageDescription
import Distribution.Simple.InstallDirs
import Distribution.Simple.Setup
import Distribution.Utils.Path
import Distribution.Version
import HsCabal
import HsCabal.HIEBios
import HsCabal.HIEBios.Pretty
import HsCabal.QQ
import Language.Haskell.Extension
import System.Directory
import System.FilePath

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

nixArchive :: IO LocalPackage
nixArchive = do
  let name = "nix-archive"
      root = "nix-archive"
      option = PerCompilerFlavor ["-O2"] []
      deps =
        [ "base" @@ [versionRangeQ| >= 4.15 && < 4.18 |],
          "filepath" ^>= [1, 4, 100],
          "bytestring" ^>= [0, 11, 2],
          "containers" ^>= [0, 6, 6],
          "binary" ^>= [0, 8]
        ]
      exts = [EnableExtension OverloadedStrings]
  lib <-
    withCurrentDirectory root $
      addLibraryMod
        ( emptyLibrary
            { libBuildInfo =
                (simpleBuildInfo "src" option)
                  { defaultExtensions = exts,
                    targetBuildDepends =
                      deps
                        ++ [ "directory" ^>= [1, 3, 8],
                             "unix" ^>= [2, 8],
                             "hashable" ^>= [1, 4, 2],
                             mkDependency
                              "template-haskell"
                              anyVersion
                              mainLibSet
                           ]
                  }
            }
        )
  testData <-
    fmap (\p -> "tests" </> "spec" </> "files" </> p)
      . S.toAscList
      <$> listDirRec (root </> "tests" </> "spec" </> "files")
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
                condTestSuites =
                  [ unConditionalTest
                      ( emptyTestSuite
                          { testName = "spec",
                            testInterface = TestSuiteExeV10 (mkVersion [1, 0]) "Main.hs",
                            testBuildInfo =
                              (simpleBuildInfo ("tests" </> "spec") option)
                                { defaultExtensions = exts,
                                  targetBuildDepends =
                                    deps
                                      ++ [ "hspec" @@ [versionRangeQ| >=2.8 && < 2.11|],
                                           anyVersionDep name
                                         ]
                                }
                          }
                      )
                  ]
              }
        }
    )

unConditionalSubLib ::
  Library ->
  (UnqualComponentName, CondTree ConfVar [Dependency] Library)
unConditionalSubLib l =
  ( fromJust (libraryNameString (libName l)),
    unConditional l
  )

simpleReexport :: ModuleName -> ModuleReexport
simpleReexport m =
  ModuleReexport
    { moduleReexportOriginalPackage = Nothing,
      moduleReexportOriginalName = m,
      moduleReexportName = m
    }

hsnixCore :: IO LocalPackage
hsnixCore =
  let name = "hsnix-core"
      root = "hsnix-core"
      commonDep =
        [ "base" @@ [versionRangeQ| >= 4.15 && < 4.18 |],
          "text" ^>= [2, 0],
          "bytestring" ^>= [0, 11],
          "hashable" ^>= [1, 4, 2]
        ]
      commonExt =
        [ EnableExtension OverloadedStrings,
          EnableExtension Strict
        ]
   in withCurrentDirectory root $ do
        backendType <-
          addLibraryMod
            ( emptyLibrary
                { libName = LSubLibName "backend-types",
                  libVisibility = LibraryVisibilityPublic,
                  libBuildInfo =
                    (simpleBuildInfo "backend-types" mempty)
                      { defaultExtensions = commonExt,
                        targetBuildDepends =
                          commonDep
                            ++ [ "memory" @@ [versionRangeQ| >= 0.16 && < 0.19|],
                                 "cryptonite" @@ [versionRangeQ| >= 0.29 && < 0.31|],
                                 "template-haskell" @@ [versionRangeQ| >= 2.17 && < 2.20 |]
                               ]
                      }
                }
            )
        lib <-
          addLibraryMod
            ( emptyLibrary
                { reexportedModules =
                    mapMaybe
                      ( \m ->
                          if "Internal" `elem` components m -- not reexport internal modules
                            then Nothing
                            else Just (simpleReexport m)
                      )
                      (exposedModules backendType),
                  libBuildInfo =
                    (simpleBuildInfo "src" mempty)
                      { defaultExtensions = commonExt,
                        targetBuildDepends =
                          commonDep
                            ++ [ "unordered-containers" ^>= [0, 2],
                                 "vector" ^>= [0, 13],
                                 "mtl" ^>= [2, 2],
                                 anyVersionDep "nix-archive",
                                 Dependency
                                  name
                                  anyVersion
                                  (NES.singleton (libName backendType))
                               ]
                      }
                }
            )
        pure
          ( LocalPackage
              { lpName = name,
                lpRoot = root,
                lpDesc =
                  emptyGenericPackageDescription
                    { packageDescription = simplePkgDesc name [0, 1, 0, 0],
                      condLibrary = Just (unConditional lib),
                      condSubLibraries = [unConditionalSubLib backendType]
                    }
              }
          )

main :: IO ()
main = do
  pkgs <- sequence [nixArchive, hsnixCore]
  traverse_ (writeLocalPackage ".") pkgs
  writeProjectConfigFile
    "cabal.project"
    ( (simpleCabalProject pkgs)
        { projectConfigLocalPackages =
            mempty
              { packageConfigTestShowDetails = toFlag Streaming,
                packageConfigTestTestOptions = [toPathTemplate "--color"]
              }
        }
    )
  simpleCradleConfig
    @()
    PbCabal
    [ "base",
      "filepath",
      "directory",
      "Cabal",
      "Cabal-syntax",
      "cabal-install",
      "containers"
    ]
    ["-Wall"]
    >>= writeCradleConfig "."