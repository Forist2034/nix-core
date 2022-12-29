module HsNix.Derivation.Drv.Internal.Nar
  ( buildNar,
    makeNarPath,
  )
where

import qualified Crypto.Hash as H
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Data.Semigroup
import qualified Data.Text.Encoding as TE
import HsNix.Builtin.AddFile
import System.Nix.ReadonlyStore
import System.Nix.StorePath

int :: Integral a => a -> BSB.Builder
int a = BSB.int64LE (fromIntegral a)

str :: BS.ByteString -> BSB.Builder
str s =
  let l = BS.length s
   in int l <> BSB.byteString s <> pad ((8 - l) `mod` 8)
  where
    pad 0 = mempty
    pad l = stimes l (BSB.word8 0)

parens :: BSB.Builder -> BSB.Builder
parens v = lp <> v <> rp
  where
    lp = str "("
    rp = str ")"

buildNar :: DirTree -> ByteString
buildNar dt =
  BSB.toLazyByteString
    (str "nix-archive-1" <> goP dt)
  where
    goP = parens . go
    go (Regular e c) =
      mconcat
        [ str "type",
          str "regular",
          case e of
            Executable -> str "executable"
            NonExecutable -> mempty,
          str "contents",
          str c
        ]
    go (SymLink t) =
      mconcat
        [ str "type",
          str "symlink",
          str "target",
          str (TE.encodeUtf8 t)
        ]
    go (Dir t) =
      mconcat
        [ str "type",
          str "directory",
          M.foldMapWithKey
            ( \n d ->
                str "entry"
                  <> parens
                    ( mconcat
                        [ str "name",
                          str (TE.encodeUtf8 n),
                          str "node",
                          goP d
                        ]
                    )
            )
            t
        ]

makeNarPath :: FilePath -> StorePathName -> DirTree -> StorePath
makeNarPath s n d =
  makeFixedOutputPath
    s
    True
    (H.hashWith H.SHA256 (LBS.toStrict (buildNar d)))
    n