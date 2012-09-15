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

-- | Apply a transform to a sink.
transformSink :: Monad m => (Sink b m r -> Sink a m r) -> Sink b m r -> Sink a m r
transformSink t = t

-- | Apply a transform to a sink.
(=$) :: Monad m => (Sink b m r -> Sink a m r) -> Sink b m r -> Sink a m r
(=$) t = t
infixl 2 =$


-- | Apply a transform to a Source.
transformSource :: (Source src, Monad m) => (forall r . Sink b m r -> Sink a m r) -> src m a -> SimpleSource m b
transformSource t src = SimpleSource $ transfer src . transformSink t

-- | Apply a transform to a source.
($=) :: (Source src, Monad m) => src m a -> (forall r . Sink b m r -> Sink a m r) -> SimpleSource m b
($=) = flip transformSource
infixl 1 $=


-- | Merges two transforms into one.
mergeTransform :: Monad m => (Sink b m r -> Sink a m r) -> (Sink c m r -> Sink b m r) -> Sink c m r -> Sink a m r
mergeTransform t1 t2 = t1 . t2

-- | Merges two transforms into one.
(=$=) :: Monad m => (Sink b m r -> Sink a m r) -> (Sink c m r -> Sink b m r) -> Sink c m r -> Sink a m r
(=$=) = mergeTransform
