module Lib.HsNixCore (hsnixCore) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.Version
import HsCabal
import HsCabal.QQ
import Language.Haskell.Extension
import System.Directory
import Utils

hsnixCore :: IO LocalPackage
hsnixCore =
  let name = "hsnix-core"
      root = "hsnix-core"
      commonDep =
        [ "base" @@ [versionRangeQ| >= 4.15 && < 4.18 |],
          "text" ^>= [2, 0],
          "hashable" ^>= [1, 4, 2]
        ]
      bytestring = "bytestring" ^>= [0, 11]
   in withCurrentDirectory root $ do
        coreTypes <-
          addLibraryMod
            ( emptyLibrary
                { libName = LSubLibName "core-types",
                  libVisibility = LibraryVisibilityPublic,
                  libBuildInfo =
                    (simpleBuildInfo "core-types" mempty)
                      { defaultExtensions = [EnableExtension OverloadedStrings],
                        targetBuildDepends =
                          commonDep
                            ++ [ bytestring,
                                 "memory" @@ [versionRangeQ| >= 0.16 && < 0.19|],
                                 "cryptonite" @@ [versionRangeQ| >= 0.29 && < 0.31|],
                                 "template-haskell" @@ [versionRangeQ| >= 2.17 && < 2.20 |]
                               ]
                      }
                }
            )
        storepath <-
          addLibraryMod
            ( emptyLibrary
                { libName = LSubLibName "storepath-sig",
                  libVisibility = LibraryVisibilityPublic,
                  libBuildInfo =
                    (simpleBuildInfo "storepath-sig" mempty)
                      { defaultExtensions = [],
                        targetBuildDepends = commonDep
                      }
                }
            )
        derivTypes <-
          addLibraryMod
            ( emptyLibrary
                { libName = LSubLibName "derivation-types",
                  libVisibility = LibraryVisibilityPublic,
                  libBuildInfo =
                    (simpleBuildInfo "derivation-types" mempty)
                      { defaultExtensions = [EnableExtension OverloadedStrings],
                        targetBuildDepends =
                          commonDep
                            ++ [ Dependency
                                  name
                                  anyVersion
                                  (NES.singleton (libName coreTypes))
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
                      (exposedModules coreTypes),
                  libBuildInfo =
                    (simpleBuildInfo "src" mempty)
                      { defaultExtensions =
                          [ EnableExtension Strict,
                            EnableExtension OverloadedStrings
                          ],
                        targetBuildDepends =
                          commonDep
                            ++ [ bytestring,
                                 "unordered-containers" ^>= [0, 2],
                                 "vector" ^>= [0, 13],
                                 "mtl" ^>= [2, 2],
                                 anyVersionDep "nix-archive",
                                 Dependency
                                  name
                                  anyVersion
                                  ( NES.fromNonEmpty
                                      ( libName coreTypes
                                          :| [ libName storepath,
                                               libName derivTypes
                                             ]
                                      )
                                  )
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
                      condSubLibraries =
                        unConditionalSubLib
                          <$> [coreTypes, storepath, derivTypes]
                    }
              }
          )