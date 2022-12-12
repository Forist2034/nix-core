{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module HsNix.Derivation
  ( DerivationArg,
    module D,
    DrvStrSpan (..),
    DrvStr (..),
    DrvStrBuilder,
    fromDrvStr,
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
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
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

data SpanBuilder m
  = SpanStr LTB.Builder
  | SpanDrv (StorePath m)

newtype DrvStrBuilder m = DB ([SpanBuilder m] -> [SpanBuilder m])

instance IsString (DrvStrBuilder m) where
  fromString s = DB (SpanStr (fromString s) :)
  {-# INLINE fromString #-}

instance Semigroup (DrvStrBuilder m) where
  DB l <> DB r = DB (l . r)

instance Monoid (DrvStrBuilder m) where
  mempty = DB id

fromDrvStr :: DrvStr m -> DrvStrBuilder m
fromDrvStr (DStr s) =
  DB
    ( \back ->
        foldr
          ( \i ss -> case i of
              Str st -> SpanStr (LTB.fromText st) : ss
              Drv d -> SpanDrv d : ss
          )
          back
          s
    )

class AsDrvStr s m where
  toDrvStr :: s -> DrvStr m

instance AsDrvStr Text m where
  toDrvStr t = DStr [Str t]

instance AsDrvStr (StorePath m) m where
  toDrvStr t = DStr [Drv t]

instance AsDrvStr (DrvStrBuilder m) m where
  toDrvStr (DB db) =
    DStr
      ( fmap
          ( \case
              SpanStr s -> Str (LT.toStrict (LTB.toLazyText s))
              SpanDrv d -> Drv d
          )
          (mergeT (db []))
      )
    where
      mergeT [] = []
      mergeT (d@(SpanDrv _) : ps) = d : mergeT ps
      mergeT (SpanStr s : ps) =
        case mergeT ps of
          (SpanStr ss : pss) -> SpanStr (s <> ss) : pss
          v -> SpanStr s : v

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