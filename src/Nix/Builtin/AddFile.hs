module Nix.Builtin.AddFile (BuiltinAddFile (..)) where

import Data.Text (Text)
import Nix.Derivation

class (MonadDeriv m) => BuiltinAddFile m where
  addFile :: Text -> Text -> m (StorePath m)