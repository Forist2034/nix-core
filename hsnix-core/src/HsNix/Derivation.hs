{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HsNix.Derivation
  ( DerivationArg,
    module D,
    AsDrvStr (..),
    Quoted (..),
    IsDrvStr (..),
    MonadDeriv (..),
    storePathStrOf,
    storePath,
    storePathStr,
  )
where

import Data.Hashable (Hashable)
import Data.String
import Data.Text (Text)
import HsNix.Internal.Derivation as D hiding (DerivationArg)
import qualified HsNix.Internal.Derivation as ID

class AsDrvStr s m where
  toDrvStr :: s -> m

type DerivationArg m = ID.DerivationArg (DrvStr m)

type DrvValue m = (Eq m, Hashable m, Show m)

type StrValue m = (IsString m, Monoid m)

data Quoted s
  = QStr s
  | QEscape Char

class
  ( DrvValue m,
    StrValue m,
    AsDrvStr Text m,
    StrValue (DrvStrBuilder m),
    AsDrvStr (DrvStrBuilder m) m
  ) =>
  IsDrvStr m
  where
  data DrvStrBuilder m
  fromDrvStr :: m -> DrvStrBuilder m
  quote :: (Char -> Bool) -> m -> [Quoted m]

class
  ( Monad m,
    IsDrvStr (DrvStr m),
    DrvValue (StorePath m),
    AsDrvStr (StorePath m) (DrvStr m),
    DrvValue (Derivation m)
  ) =>
  MonadDeriv m
  where
  data DrvStr m
  data StorePath m
  data Derivation m
  data BuildResult m
  derivation :: m (DerivationArg m) -> Derivation m
  storePathOf :: Derivation m -> Maybe Text -> m (StorePath m)
  build :: Derivation m -> BuildResult m

storePathStrOf :: MonadDeriv m => Derivation m -> Maybe Text -> m (DrvStr m)
storePathStrOf d o = toDrvStr <$> storePathOf d o

storePath :: MonadDeriv m => Derivation m -> m (StorePath m)
storePath d = storePathOf d Nothing

storePathStr :: MonadDeriv m => Derivation m -> m (DrvStr m)
storePathStr d = toDrvStr <$> storePath d