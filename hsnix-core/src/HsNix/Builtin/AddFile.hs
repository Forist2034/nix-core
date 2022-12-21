module HsNix.Builtin.AddFile
  ( BuiltinAddText (..),
    addTextFileStr,
    BuiltinAddBinary (..),
    addBinFileStr,
    BuiltinAddDrvStr (..),
    addDStrStr,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import HsNix.Derivation

class (MonadDeriv m) => BuiltinAddText m where
  addTextFile :: Text -> Text -> m (StorePath m)

addTextFileStr :: BuiltinAddText m => Text -> Text -> m (DrvStr m)
addTextFileStr n c = toDrvStr <$> addTextFile n c

class (MonadDeriv m) => BuiltinAddBinary m where
  addBinaryFile :: Text -> ByteString -> m (StorePath m)

addBinFileStr :: BuiltinAddBinary m => Text -> ByteString -> m (DrvStr m)
addBinFileStr n b = toDrvStr <$> addBinaryFile n b

class (MonadDeriv m) => BuiltinAddDrvStr m where
  addDrvStr :: Text -> DrvStr m -> m (StorePath m)

addDStrStr :: BuiltinAddDrvStr m => Text -> DrvStr m -> m (DrvStr m)
addDStrStr n c = toDrvStr <$> addDrvStr n c