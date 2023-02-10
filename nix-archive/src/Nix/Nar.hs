{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Nix.Nar
  ( Executable (..),
    NarEntry (..),
    Nar (..),
    readNar,
    writeNar,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Foldable
import Data.Functor
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as M
import Data.Semigroup
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)
import System.Posix.ByteString.FilePath
import System.Posix.Directory.ByteString
import System.Posix.Files.ByteString
import System.Posix.IO.ByteString

data Executable
  = Executable
  | NonExecutable
  deriving (Show, Eq, Ord, Lift, Generic)

instance Hashable Executable

padCount :: Int -> Int
padCount l = (8 - l) `mod` 8

putPad :: Int -> PutM ()
putPad 0 = pure ()
putPad n = stimes n (putWord8 0)

putBS :: ByteString -> Put
putBS bs =
  let l = BS.length bs
   in do
        putInt64le (fromIntegral l)
        putByteString bs
        putPad (padCount l)

putParens :: Put -> Put
putParens v = putBS "(" <> v <> putBS ")"

getBS :: Get ByteString
getBS = do
  l <- fromIntegral <$> getInt64le
  getByteString l <* skip (padCount l)

getBSConst :: ByteString -> Get ()
getBSConst ex = do
  bs <- getBS
  unless
    (bs == ex)
    (fail ("Invalid content. Expected " <> show ex <> ", but get " <> show bs))

getParens :: Get b -> Get b
getParens v = getBSConst "(" >> (v <* getBSConst ")")

data NarEntry
  = Regular Executable ByteString
  | Directory (M.Map RawFilePath NarEntry)
  | SymLink RawFilePath
  deriving (Show, Eq, Ord, Lift, Generic)

instance Hashable NarEntry

instance Binary NarEntry where
  put ne =
    putParens
      ( do
          putBS "type"
          case ne of
            Regular e c -> do
              putBS "regular"
              when (e == Executable) $ do
                putBS "executable"
                putBS ""
              putBS "contents"
              putBS c
            Directory ds -> do
              putBS "directory"
              traverse_
                ( \(e, ent) -> do
                    putBS "entry"
                    putParens
                      ( do
                          putBS "name"
                          putBS e
                          putBS "node"
                          put ent
                      )
                )
                (M.toAscList ds)
            SymLink t -> do
              putBS "symlink"
              putBS "target"
              putBS t
      )
  get =
    getParens
      ( do
          getBSConst "type"
          getBS >>= \case
            "regular" -> do
              exec <-
                getBS >>= \case
                  "executable" -> do
                    getBSConst ""
                    getBSConst "contents" $> Executable
                  "contents" -> pure NonExecutable
                  t -> fail ("Invalid executable spec in regular file entry. " <> show t)
              Regular exec <$> getBS
            "directory" -> do
              Directory . M.fromList
                <$> many
                  ( getBSConst "entry"
                      >> getParens
                        ( do
                            getBSConst "name"
                            n <- getBS
                            getBSConst "node"
                            ent <- get
                            pure (n, ent)
                        )
                  )
            "symlink" -> do
              getBSConst "target"
              SymLink <$> getBS
            t -> fail ("Invalid entry type " <> show t)
      )

newtype Nar = Nar {narRoot :: NarEntry}
  deriving (Show, Eq, Lift, Generic)

instance Hashable Nar

instance Binary Nar where
  put (Nar n) = putBS "nix-archive-1" >> put n
  get = getBSConst "nix-archive-1" >> Nar <$> get

withCurrentDirectory :: RawFilePath -> IO a -> IO a
withCurrentDirectory cwd f =
  bracket
    (getWorkingDirectory <* changeWorkingDirectory cwd)
    changeWorkingDirectory
    (const f)

readFileRaw :: Int -> RawFilePath -> IO ByteString
readFileRaw sz fp =
  bracket
    (openFd fp ReadOnly Nothing defaultFileFlags)
    closeFd
    ( \fd -> do
        buf <- mallocBytes sz
        void (fdReadBuf fd buf (fromIntegral sz))
        BSU.unsafePackMallocCStringLen (castPtr buf, sz)
    )

listDirectory :: RawFilePath -> IO [RawFilePath]
listDirectory fp =
  bracket
    (openDirStream fp)
    closeDirStream
    (go [])
  where
    go l d =
      readDirStream d >>= \p ->
        if BS.null p
          then pure l
          else go (if p /= "." && p /= ".." then p : l else l) d

readNar :: RawFilePath -> IO Nar
readNar = fmap Nar . go
  where
    go fp = do
      stat <- getSymbolicLinkStatus fp
      if
          | isRegularFile stat ->
              Regular
                ( if fileMode stat
                    `intersectFileModes` ownerExecuteMode
                    == nullFileMode
                    then NonExecutable
                    else Executable
                )
                <$> readFileRaw (fromIntegral (fileSize stat)) fp
          | isDirectory stat ->
              Directory . M.fromList
                <$> withCurrentDirectory
                  fp
                  (listDirectory "." >>= traverse (\p -> (p,) <$> go p))
          | isSymbolicLink stat -> SymLink <$> readSymbolicLink fp
          | otherwise -> error ("Unsupported file: " <> show fp)

writeNar :: RawFilePath -> Nar -> IO ()
writeNar fp (Nar nar) = go fp nar
  where
    go n (Regular e c) =
      bracket
        ( createFile
            n
            ( case e of
                Executable ->
                  stdFileMode `unionFileModes` ownerExecuteMode
                NonExecutable -> stdFileMode
            )
        )
        closeFd
        ( \fd -> do
            h <- fdToHandle fd
            BS.hPut h c
        )
    go n (Directory ent) =
      createDirectory n accessModes
        >> withCurrentDirectory n (traverse_ (uncurry go) (M.toList ent))
    go n (SymLink t) = createSymbolicLink t n
