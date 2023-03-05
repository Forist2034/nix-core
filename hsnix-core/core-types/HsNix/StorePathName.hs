module HsNix.StorePathName (
  StorePathName (storePathNameText),
  validStorePathName,
  makeStorePathName,
  makeStorePathNameThrow,
) where

import HsNix.Internal.StorePathName
