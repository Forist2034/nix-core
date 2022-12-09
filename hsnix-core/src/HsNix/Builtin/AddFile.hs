module HsNix.Builtin.AddFile
  ( BuiltinAddFile (..),
    addText,
    addFileStr,
    addTextStr,
  )
where

import Data.Text (Text)
import HsNix.Derivation

class (MonadDeriv m) => BuiltinAddFile m where
  addFile :: Text -> DrvStr m -> m (StorePath m)

addText :: (BuiltinAddFile m) => Text -> Text -> m (StorePath m)
addText n c = addFile n (fromText c)

addFileStr :: BuiltinAddFile m => Text -> DrvStr m -> m (DrvStr m)
addFileStr n c = pathToStr <$> addFile n c

addTextStr :: BuiltinAddFile m => Text -> Text -> m (DrvStr m)
addTextStr n c = addFileStr n (fromText c)