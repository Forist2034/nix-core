{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module HsNix.Derivation
  ( DerivationArg,
    module D,
    IsDrvStr (..),
    (</>),
    MonadDeriv (..),
    storePathStrOf,
    storePath,
    storePathStr,
  )
where

import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text (Text)
import HsNix.Internal.Derivation as D hiding (DerivationArg)
import qualified HsNix.Internal.Derivation as ID

type DrvValue m = (Eq m, Hashable m, Show m)

class (IsString m, Monoid m, DrvValue m) => IsDrvStr m where
  fromText :: Text -> m

(</>) :: (IsDrvStr m) => m -> m -> m
l </> r = l <> "/" <> r

type DerivationArg m = ID.DerivationArg (DrvStr m) (Derivation m)

class
  (Monad m, IsDrvStr (DrvStr m), DrvValue (StorePath m), DrvValue (Derivation m)) =>
  MonadDeriv m
  where
  data DrvStr m
  data StorePath m
  data Derivation m
  data BuildResult m
  derivation :: m (DerivationArg m) -> Derivation m
  pathToStr :: StorePath m -> DrvStr m
  storePathOf :: Derivation m -> Maybe Text -> m (StorePath m)
  build :: Derivation m -> BuildResult m

storePathStrOf :: MonadDeriv m => Derivation m -> Maybe Text -> m (DrvStr m)
storePathStrOf d o = pathToStr <$> storePathOf d o

storePath :: MonadDeriv m => Derivation m -> m (StorePath m)
storePath d = storePathOf d Nothing

storePathStr :: MonadDeriv m => Derivation m -> m (DrvStr m)
storePathStr d = pathToStr <$> storePath d