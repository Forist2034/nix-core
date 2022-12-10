{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module HsNix.Derivation
  ( DerivationArg,
    module D,
    DrvStrSpan (..),
    DrvStr (..),
    AsDrvStr (..),
    (</>),
    MonadDeriv (..),
    storePathStrOf,
    storePath,
    storePathStr,
  )
where

import Data.Hashable (Hashable (hashWithSalt))
import Data.String
import Data.Text (Text)
import HsNix.Internal.Derivation as D hiding (DerivationArg)
import qualified HsNix.Internal.Derivation as ID

data DrvStrSpan m
  = Str Text
  | Drv (StorePath m)

deriving instance (MonadDeriv m) => Eq (DrvStrSpan m)

deriving instance (MonadDeriv m) => Show (DrvStrSpan m)

instance (MonadDeriv m) => Hashable (DrvStrSpan m) where
  hashWithSalt s (Str t) = hashWithSalt s (True, t)
  hashWithSalt s (Drv t) = hashWithSalt s (False, t)

newtype DrvStr m = DStr [DrvStrSpan m]
  deriving (Eq, Show)

deriving newtype instance (MonadDeriv m) => Hashable (DrvStr m)

instance IsString (DrvStr m) where
  fromString s = DStr [Str (fromString s)]
  {-# INLINE fromString #-}

instance Semigroup (DrvStr m) where
  DStr [] <> r = r
  l <> DStr [] = l
  DStr l <> DStr r@(r1 : rs) =
    let ll = last l
     in DStr
          ( case (ll, r1) of
              (Str sl, Str sr) -> init l ++ Str (sl <> sr) : rs
              _ -> l ++ r
          )

instance Monoid (DrvStr m) where
  mempty = DStr []

class (MonadDeriv m) => AsDrvStr s m where
  toDrvStr :: s -> DrvStr m

instance (MonadDeriv m) => AsDrvStr Text m where
  toDrvStr t = DStr [Str t]

instance (MonadDeriv m) => AsDrvStr (StorePath m) m where
  toDrvStr t = DStr [Drv t]

(</>) :: (AsDrvStr t1 m, AsDrvStr t2 m) => t1 -> t2 -> DrvStr m
l </> r = toDrvStr l <> "/" <> toDrvStr r

type DerivationArg m = ID.DerivationArg (DrvStr m)

type DrvValue m = (Eq m, Hashable m, Show m)

class
  (Monad m, DrvValue (StorePath m), DrvValue (Derivation m)) =>
  MonadDeriv m
  where
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