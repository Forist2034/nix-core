{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TupleSections #-}

module HsNix.Builtin.AddFile
  ( BuiltinAddText (..),
    addTextFileStr,
    BuiltinAddBinary (..),
    addBinFileStr,
    IsExecutable (..),
    DirTree (..),
    textFile,
    mkDir,
    readDirTree,
    writeDirTree,
    BuiltinAddDir (..),
    addDirStr,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Functor
import Data.Hashable
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import HsNix.Derivation
-- for Lift Map
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)
import System.Directory

class (MonadDeriv m) => BuiltinAddText m where
  addTextFile :: Text -> Text -> m (StorePath m)

addTextFileStr :: BuiltinAddText m => Text -> Text -> m (DrvStr m)
addTextFileStr n c = toDrvStr <$> addTextFile n c

class (MonadDeriv m) => BuiltinAddBinary m where
  addBinaryFile :: Text -> ByteString -> m (StorePath m)

addBinFileStr :: BuiltinAddBinary m => Text -> ByteString -> m (DrvStr m)
addBinFileStr n b = toDrvStr <$> addBinaryFile n b

data IsExecutable
  = Executable
  | NonExecutable
  deriving (Show, Eq, Ord, Generic, Lift)

instance Hashable IsExecutable

data DirTree
  = Regular IsExecutable ByteString
  | SymLink Text
  | Dir (M.Map Text DirTree)
  deriving (Show, Eq, Ord, Generic, Lift)

instance Hashable DirTree

textFile :: IsExecutable -> Text -> DirTree
textFile e v = Regular e (TE.encodeUtf8 v)

mkDir :: [(Text, DirTree)] -> DirTree
mkDir =
  Dir
    . M.fromListWithKey
      (\k _ _ -> error ("duplicate dir entry " ++ T.unpack k))

readDirTree :: FilePath -> IO DirTree
readDirTree p =
  ( check
      (pathIsSymbolicLink p)
      (getSymbolicLinkTarget p <&> SymLink . T.pack)
      . check
        (doesDirectoryExist p)
        ( withCurrentDirectory
            p
            ( listDirectory "."
                >>= traverse (\f -> fmap (T.pack f,) (readDirTree f))
                <&> Dir . M.fromList
            )
        )
  )
    $ do
      c <- BS.readFile p
      perm <- getPermissions p
      pure
        ( Regular
            ( if executable perm
                then Executable
                else NonExecutable
            )
            c
        )
  where
    check pr t f =
      pr >>= \r -> if r then t else f

writeDirTree :: FilePath -> Text -> DirTree -> IO ()
writeDirTree root name dt =
  withCurrentDirectory root (go (T.unpack name) dt)
  where
    go fp (Regular e c) = do
      BS.writeFile fp c
      setPermissions
        fp
        emptyPermissions
          { readable = True,
            executable = e == Executable
          }
    go fp (SymLink d) =
      let p = T.unpack d
       in doesDirectoryExist p >>= \dir ->
            if dir
              then createDirectoryLink p fp
              else createFileLink p fp
    go fp (Dir d) =
      createDirectory fp
        >> withCurrentDirectory
          fp
          (void (M.traverseWithKey (go . T.unpack) d))

class (MonadDeriv m) => BuiltinAddDir m where
  addDirectory :: Text -> DirTree -> m (StorePath m)

addDirStr :: BuiltinAddDir m => Text -> DirTree -> m (DrvStr m)
addDirStr n d = toDrvStr <$> addDirectory n d