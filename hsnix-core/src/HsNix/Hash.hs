module HsNix.Hash
  ( HashAlgo (..),
    HashMode (..),
    Hash (..),
  )
where

import Data.Hashable
import Data.Text (Text)

data HashAlgo
  = HashSha1
  | HashSha256
  | HashSha512
  deriving (Show, Eq)

instance Hashable HashAlgo where
  hashWithSalt s v =
    hashWithSalt
      s
      ( ( case v of
            HashSha1 -> 0
            HashSha256 -> 1
            HashSha512 -> 2
        ) ::
          Int
      )

data HashMode
  = HashFlat
  | HashRecursive
  deriving (Show, Eq)

instance Hashable HashMode where
  hashWithSalt s v = hashWithSalt s (v == HashFlat)

newtype Hash = Hash Text
  deriving (Show, Eq)

instance Hashable Hash where
  hashWithSalt s (Hash h) = hashWithSalt s h