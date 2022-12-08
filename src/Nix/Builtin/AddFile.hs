module Nix.Builtin.AddFile
  ( BuiltinAddFile (..),
    addFileStr,
  )
where

import Data.Text (Text)
import Nix.Derivation

class (MonadDeriv m) => BuiltinAddFile m where
  addFile :: Text -> Text -> m (StorePath m)

addFileStr :: BuiltinAddFile m => Text -> Text -> m (DrvStr m)
addFileStr n c = pathToStr <$> addFile n c