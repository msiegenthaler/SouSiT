{-# LANGUAGE RankNTypes #-}

module Data.SouSiT.Transform (
  Transform,
  mergeTransform,
  transformSink,
  transformSource,
  (=$=),
  (=$),
  ($=)
) where

import Data.SouSiT.Sink
import Data.SouSiT.Source

type Transform a b = forall m r . Monad m => Sink b m r -> Sink a m r

-- | apply a transform to a sink
transformSink :: Monad m => (Sink b m r -> Sink a m r) -> Sink b m r -> Sink a m r
transformSink t = t

-- | apply a transform to a sink
(=$) :: Monad m => (Sink b m r -> Sink a m r) -> Sink b m r -> Sink a m r
(=$) t = t
infixl 2 =$


-- | apply a transform to a source
--transformSource :: (Source src, Monad m) => Transform a b -> src m a -> BasicSource m b
transformSource :: (Source src, Monad m) => (forall r . Sink b m r -> Sink a m r) -> src m a -> BasicSource m b
transformSource t src = BasicSource $ transfer src . transformSink t

-- | apply a transform to a source
($=) :: (Source src, Monad m) => src m a -> (forall r . Sink b m r -> Sink a m r) -> BasicSource m b
($=) = flip transformSource
infixl 1 $=


-- | merges two transforms into one
mergeTransform :: Monad m => (Sink b m r -> Sink a m r) -> (Sink c m r -> Sink b m r) -> (Sink c m r -> Sink a m r)
mergeTransform t1 t2 = t1 . t2

-- | merges two transforms into one
(=$=) :: Monad m => (Sink b m r -> Sink a m r) -> (Sink c m r -> Sink b m r) -> (Sink c m r -> Sink a m r)
(=$=) = mergeTransform
