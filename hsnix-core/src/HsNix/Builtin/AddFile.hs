module HsNix.Builtin.AddFile
  ( BuiltinAddText (..),
    addTextFileStr,
    BuiltinAddBinary (..),
    addBinFileStr,
    BuiltinAddNar (..),
    addNarStr,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import HsNix.Derivation
import Nix.Nar

class (ApplicativeDeriv m) => BuiltinAddText m where
  addTextFile :: Text -> Text -> m (StorePath m)

addTextFileStr :: BuiltinAddText m => Text -> Text -> m (DrvStr m)
addTextFileStr n c = toDrvStr <$> addTextFile n c

class (ApplicativeDeriv m) => BuiltinAddBinary m where
  addBinaryFile :: Text -> ByteString -> m (StorePath m)

addBinFileStr :: BuiltinAddBinary m => Text -> ByteString -> m (DrvStr m)
addBinFileStr n b = toDrvStr <$> addBinaryFile n b

class (ApplicativeDeriv m) => BuiltinAddNar m where
  addNar :: Text -> Nar -> m (StorePath m)

addNarStr :: BuiltinAddNar m => Text -> Nar -> m (DrvStr m)
addNarStr n d = toDrvStr <$> addNar n d