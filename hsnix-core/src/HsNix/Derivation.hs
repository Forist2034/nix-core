{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HsNix.Derivation
  ( DerivationArg,
    module D,
    AsDrvStr (..),
    Quoted (..),
    HasStrBuilder (..),
    ApplicativeDeriv (..),
    storePathStrOf,
    storePath,
    storePathStr,
  )
where

import Data.Hashable (Hashable)
import Data.String
import Data.Text (Text)
import HsNix.Hash
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
  ( StrValue (DrvStrBuilder m),
    AsDrvStr (DrvStrBuilder m) (DrvStr m)
  ) =>
  HasStrBuilder m
  where
  data DrvStrBuilder m
  fromDrvStr :: DrvStr m -> DrvStrBuilder m
  quote :: (Char -> Bool) -> DrvStr m -> [Quoted (DrvStr m)]

-- | Only use applicative not monad.
--    Avoid missing dependencies like
--    @
--      derivation $ do
--        x <- d1
--        y <- storePathStr
--          (derivation (pure (defaultDrvArg "foo" y "bar")))
--        -- dependent x not recorded in y
--        pure (defaultDrvArg "a" y "x")
--    @
class
  ( Applicative m,
    DrvValue (DrvStr m),
    StrValue (DrvStr m),
    AsDrvStr Text (DrvStr m),
    HasStrBuilder m,
    DrvValue (StorePath m),
    AsDrvStr (StorePath m) (DrvStr m),
    DrvValue (Derivation m)
  ) =>
  ApplicativeDeriv m
  where
  data DrvStr m
  data StorePath m
  data Derivation m
  data BuildResult m
  derivation :: NamedHashAlgo a => m (DerivationArg m a) -> Derivation m
  storePathOf :: Derivation m -> Maybe Text -> m (StorePath m)
  build :: Derivation m -> BuildResult m

storePathStrOf :: ApplicativeDeriv m => Derivation m -> Maybe Text -> m (DrvStr m)
storePathStrOf d o = toDrvStr <$> storePathOf d o

storePath :: ApplicativeDeriv m => Derivation m -> m (StorePath m)
storePath d = storePathOf d Nothing

storePathStr :: ApplicativeDeriv m => Derivation m -> m (DrvStr m)
storePathStr d = toDrvStr <$> storePath d