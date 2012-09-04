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
transformSink :: Monad m => Transform a b -> Sink b m r -> Sink a m r
transformSink t = t

-- | apply a transform to a sink
(=$) :: Monad m => Transform a b -> Sink b m r -> Sink a m r
(=$) = transformSink
infixl 2 =$


-- | apply a transform to a source
transformSource :: (Source src, Monad m) => Transform a b -> src m a -> BasicSource m b
transformSource t src = BasicSource $ transfer src . transformSink t

-- | apply a transform to a source
($=) :: (Source src, Monad m) => src m a -> Transform a b -> BasicSource m b
($=) = flip transformSource
infixl 1 $=


-- | merges two transforms into one
mergeTransform :: Transform a b -> Transform b c -> Transform a c
mergeTransform t1 t2 = t1 . t2

-- | merges two transforms into one
(=$=) :: Transform a b -> Transform b c -> Transform a c
(=$=) = mergeTransform
