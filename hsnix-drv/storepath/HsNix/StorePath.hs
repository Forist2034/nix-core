{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HsNix.StorePath (
  StorePath (..),
  storePathStr,
) where

import Data.Coerce
import Data.Hashable
import Data.Text (Text)
import HsNix.DrvStr

newtype StorePath = StorePath {storePathText :: Text}
  deriving (Show, Eq, Hashable)

storePathStr :: StorePath -> DrvStr
storePathStr = coerce . storePathText
