{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HsNix.Internal.System
  ( System (..),
    x86_64_linux,
    i686_linux,
  )
where

import Data.Hashable (Hashable)
import Data.Text (Text)
import Language.Haskell.TH.Syntax (Lift)

newtype System = System {systemName :: Text}
  deriving (Show, Eq, Hashable, Lift)

x86_64_linux :: System
x86_64_linux = System "x86_64-linux"

i686_linux :: System
i686_linux = System "i686-linux"