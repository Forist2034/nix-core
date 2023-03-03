{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import qualified Data.Map.Strict as M
import Nix.Nar
import System.OsPath.Posix
import Test.Hspec

type Case = (String, Nar, FilePath)

testCases :: [Case]
testCases =
  [ ( "regular nonexcutable file",
      Nar (Regular NonExecutable "123\n"),
      "tests/spec/files/regular_file.nar"
    ),
    ( "regular executable file",
      Nar (Regular Executable "123\n"),
      "tests/spec/files/executable_file.nar"
    ),
    ( "symbol link",
      Nar (SymLink [pstr|../s|]),
      "tests/spec/files/sym_link.nar"
    ),
    ( "simple directory",
      Nar
        ( Directory
            ( M.fromAscList
                [ ([pstr|k1|], Regular NonExecutable mempty),
                  ([pstr|k2|], Regular NonExecutable "123456"),
                  ([pstr|k3|], Regular Executable "123\n456\n")
                ]
            )
        ),
      "tests/spec/files/simple_dir.nar"
    )
  ]

main :: IO ()
main = hspec $ do
  describe
    "serialize"
    ( traverse_
        (\(desc, na, fp) -> it desc (LBS.readFile fp >>= (encode na `shouldBe`)))
        testCases
    )
  describe
    "deserialize"
    ( traverse_
        (\(desc, na, fp) -> it desc (decodeFile fp `shouldReturn` na))
        testCases
    )
