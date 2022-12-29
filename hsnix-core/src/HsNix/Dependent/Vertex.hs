{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HsNix.Dependent.Vertex
  ( DepVertT,
    addEdge,
    runDepVertT,
    execDepVertT,
    DepVert,
    runDepVert,
    execDepVert,
  )
where

import Control.Monad.Identity
import Control.Monad.Writer

newtype DepVertT c m a = DV (WriterT c m a)
  deriving (Functor, Applicative, Monad)

instance (Monoid c) => MonadTrans (DepVertT c) where
  lift = DV . lift

addEdge :: (Monoid c, Monad m) => c -> DepVertT c m ()
addEdge = DV . tell

runDepVertT :: DepVertT c m a -> m (a, c)
runDepVertT (DV v) = runWriterT v

execDepVertT :: (Monad m) => DepVertT c m a -> m c
execDepVertT (DV v) = execWriterT v

type DepVert c = DepVertT c Identity

runDepVert :: DepVert w a -> (a, w)
runDepVert = runIdentity . runDepVertT

execDepVert :: DepVert c a -> c
execDepVert = runIdentity . execDepVertT