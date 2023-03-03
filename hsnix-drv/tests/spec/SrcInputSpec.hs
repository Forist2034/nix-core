{-# LANGUAGE QuasiQuotes #-}

module SrcInputSpec (
  SrcTest (stInput),
  textSrc,
  binarySrc,
  narSimpleDirSrc,
  spec,
) where

import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Text (Text)
import HsNix.Derivation.Backend
import HsNix.DrvDirect.Internal.Types
import HsNix.Internal.StorePathName
import Nix.Nar
import System.OsPath.Posix (pstr)
import Test.Hspec

shouldBeSrc :: SrcInput -> Text -> Expectation
shouldBeSrc s sp = srcSPText s `shouldBe` sp

data SrcTest = SrcTest
  { stName :: String,
    stInput :: SrcInput,
    stResult :: Text
  }

runSrcTest :: SrcTest -> Spec
runSrcTest st = it (stName st) (stInput st `shouldBeSrc` stResult st)

textSrc :: SrcTest
textSrc =
  SrcTest
    { stName = "text file",
      stInput = addTextFile (StorePathName "text-test") "test",
      stResult = "/nix/store/2jsq549zbk2sjd175m7rlnk218f8m3nd-text-test"
    }

binarySrc :: SrcTest
binarySrc =
  SrcTest
    { stName = "binary file",
      stInput = addBinaryFile (StorePathName "binary-test") "test",
      stResult = "/nix/store/gdymj3wr21bm9ns18m3wqnjzlyba96vz-binary-test"
    }

narSimpleDirSrc :: SrcTest
narSimpleDirSrc =
  SrcTest
    { stName = "simple directory",
      stInput =
        addNar
          (StorePathName "nar-test1")
          ( Nar
              ( Directory
                  ( M.fromList
                      [ ([pstr|regular|], Regular NonExecutable "file1"),
                        ([pstr|symlink|], SymLink [pstr|regular|]),
                        ([pstr|executable|], Regular Executable "executable file"),
                        ( [pstr|subdir|],
                          Directory
                            (M.singleton [pstr|file1|] (Regular NonExecutable "file"))
                        )
                      ]
                  )
              )
          ),
      stResult = "/nix/store/mqygs5y082n90jwjddra67nvc8als68k-nar-test1"
    }

spec :: Spec
spec =
  traverse_
    runSrcTest
    [ textSrc,
      binarySrc,
      narSimpleDirSrc
    ]