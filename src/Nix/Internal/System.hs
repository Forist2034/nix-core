module Nix.Internal.System
  ( System (..),
    systemName,
    x86_64_linux,
    i686_linux,
  )
where

import Data.Hashable
import Data.Text (Text)

newtype System = System Text
  deriving (Show, Eq)

instance Hashable System where
  hashWithSalt s (System v) = hashWithSalt s v

systemName :: System -> Text
systemName (System s) = s

x86_64_linux :: System
x86_64_linux = System "x86_64-linux"

i686_linux :: System
i686_linux = System "i686-linux"