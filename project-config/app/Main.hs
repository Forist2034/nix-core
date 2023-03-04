module Main (main) where

import Data.Foldable
import Distribution.Client.ProjectConfig
import Distribution.Client.Types.AllowNewer
import Distribution.PackageDescription
import Distribution.Simple.InstallDirs
import Distribution.Simple.Setup
import HsCabal
import Lib.HsNixCore
import Lib.HsNixDrv
import Lib.NixArchive
import System.Directory

mkRelaxDep :: PackageName -> [PackageName] -> [RelaxedDep]
mkRelaxDep p =
  fmap
    ( RelaxedDep
        (RelaxDepScopePackage p)
        RelaxDepModNone
        . RelaxDepSubjectPkg
    )

main :: IO ()
main = withCurrentDirectory ".." $ do
  pkgs <- sequence [nixArchive, hsnixCore, hsnixDrv]
  traverse_ (writeLocalPackage ".") pkgs
  writeProjectConfigFile
    "cabal.project"
    ( (simpleCabalProject pkgs)
        { projectConfigShared =
            mempty
              { projectConfigAllowNewer =
                  Just
                    ( AllowNewer
                        ( RelaxDepsSome
                            (mkRelaxDep "relude" ["base", "ghc-prim"])
                        )
                    )
              },
          projectConfigLocalPackages =
            mempty
              { packageConfigTestShowDetails = toFlag Streaming,
                packageConfigTestTestOptions = [toPathTemplate "--color"]
              }
        }
    )