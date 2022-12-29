{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HsNix.Dependent.Topo
  ( DepVertex (..),
    DepTopoM,
    addVertex,
    topoSortDep,
  )
where

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Kind
import Data.Type.Equality
import GHC.Stack (HasCallStack)

class (Eq (Id a), Hashable (Id a), Foldable (EdgeSet a)) => DepVertex a where
  type Id a
  type Id a = a
  getId :: a -> Id a
  default getId :: (a ~~ Id a) => a -> Id a
  getId = id
  type EdgeSet a :: Type -> Type
  getDep :: a -> EdgeSet a a

-- | build result list (function run in reverse order)
newtype Result a = Res {getRes :: [a] -> [a]}

instance Semigroup (Result a) where
  Res l <> Res r = Res (l . r)

instance Monoid (Result a) where
  mempty = Res id

newtype DepTopoM d a
  = DG (WriterT (Result d) (State (HM.HashMap (Id d) Bool)) a)
  deriving (Functor, Applicative, Monad)

addVertex :: (DepVertex d, HasCallStack) => d -> DepTopoM d ()
addVertex d =
  let i = getId d
   in DG (gets (HM.lookup i)) >>= \case
        Just True -> pure ()
        Just False -> error "Cycle detected when running topological sort"
        Nothing -> do
          DG (modify (HM.insert i False))
          traverse_ addVertex (getDep d)
          DG (tell (Res (d :)))
          DG (modify (HM.insert i True))

topoSortDep :: DepTopoM d () -> [d]
topoSortDep (DG d) = getRes (evalState (execWriterT d) HM.empty) []
