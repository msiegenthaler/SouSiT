{-# LANGUAGE RankNTypes, TupleSections, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Data.SouSiT (
    -- * Sink
    Sink(..),
    closeSink,
    feedSink,
    feedSinkList,
    concatSink,
    (=|=),
    appendSink,
    (=||=),
    -- * Source
    Source,
    transfer,
    feedToSink,
    ($$),
    concatSources,
    (=+=),
    (=+|=),
    BasicSource(..),
    BasicSource2(..),
    -- * Transform
    Transform(..),
    (=$),
    ($=),
    (=$=),
    ComplexTransformer(..),
    genericMerge
) where

import Data.Monoid
import Control.Monad
import Control.Applicative

--- | Sink for data. Aggregates data to produce a single result (i.e. an IO).
data Sink a m r = SinkCont (a -> m (Sink a m r)) (m r)
                | SinkDone (m r)

instance Functor m => Functor (Sink a m) where
    fmap = mapSink


mapSink :: (Functor m) => (r1 -> r2) -> Sink a m r1 -> Sink a m r2
mapSink f (SinkCont next r) = SinkCont (mapSinkF . next) (fmap f r)
    where mapSinkF = fmap (mapSink f)
mapSink f (SinkDone r)      = SinkDone $ fmap f r

-- | Close the sink and return its result
closeSink :: Sink a m r -> m r
closeSink (SinkDone r) = r
closeSink (SinkCont _ r) = r

-- | Feed an input to a sink. If the sink is already done then the input is ignored.
feedSink :: Applicative m => Sink a m r -> a -> m (Sink a m r)
feedSink (SinkCont f _) i = f i
feedSink sink _           = pure sink

-- | Feed a list of inputs to a sink.
feedSinkList :: Monad m => [a] -> Sink a m r -> m (Sink a m r)
feedSinkList []     sink              = return sink
feedSinkList _      done@(SinkDone _) = return done
feedSinkList (x:xs) (SinkCont f r)    = f x >>= feedSinkList xs


-- | Concatenates two sinks. See concatSink.
(=|=) :: Applicative m => Sink a m r1 -> Sink a m r2 -> Sink a m (r1, r2)
(=|=) = concatSink
infixl 3 =|=

-- | Concatenates two sinks.
-- After the first returns SinkDone, the data is inputed into the second.
concatSink :: Applicative m => Sink a m r1 -> Sink a m r2 -> Sink a m (r1, r2)
concatSink (SinkDone r1)    (SinkDone r2)    = SinkDone $ (,) <$> r1 <*> r2
concatSink s1@(SinkDone r1) (SinkCont f2 r2) = SinkCont next done
    where next = fmap (concatSink s1) . f2
          done = (,) <$> r1 <*> r2
concatSink (SinkCont f1 r1) s2               = SinkCont next done
    where next = fmap (`concatSink` s2) . f1
          done = (,) <$> r1 <*> closeSink s2

-- | Concatenates two sinks that produce a monoid.
(=||=) :: (Applicative m, Monoid r) => Sink a m r -> Sink a m r -> Sink a m r
(=||=) = appendSink
infixl 3 =||=

-- | Concatenates two sinks that produce a monoid.
appendSink :: (Applicative m, Monoid r) => Sink a m r -> Sink a m r -> Sink a m r
appendSink s1 s2 = fmap (uncurry mappend) $ concatSink s1 s2






-- | Something that produces data to be processed by a sink
class Source src where
    transfer :: Monad m => src m a -> Sink a m r -> m r

-- | An additional typeclass for more flexible sources (allowing i.e. concats)
class Source src => Source2 src where
    feedToSink :: Monad m => src m a -> Sink a m r -> m (Sink a m r)

-- | Transfer the data from the source into the sink
($$) :: (Source src, Monad m) => src m a -> Sink a m r -> m r
($$) = transfer
infixl 0 $$

-- | A basic instance of Source
data BasicSource m a = BasicSource (forall r. Sink a m r -> m r)
instance Source BasicSource where
    transfer (BasicSource f) = f

-- | A basic instance of Source2 (and Source)
data BasicSource2 m a = BasicSource2 (forall r. Sink a m r -> m (Sink a m r))
instance Source2 BasicSource2 where
    feedToSink (BasicSource2 f) = f
instance Source BasicSource2 where
    transfer src sink = feedToSink src sink >>= closeSink

-- | Concatenates two sources.
concatSources :: (Source2 src1, Source src2, Monad m) => src1 m a -> src2 m a -> BasicSource m a
concatSources src1 src2 = BasicSource f
    where f sink = feedToSink src1 sink >>= transfer src2

-- | Concatenates two sources yielding a Source2.
concatSources2 :: (Source2 src1, Source2 src2, Monad m) => src1 m a -> src2 m a -> BasicSource2 m a
concatSources2 src1 src2 = BasicSource2 f
    where f sink = feedToSink src1 sink >>= feedToSink src2

-- | Concatenates two sources.
(=+=) :: (Source2 src1, Source2 src2, Monad m) => src1 m a -> src2 m a -> BasicSource2 m a
(=+=) = concatSources2
infixl 3 =+=

-- | Concatenates two sources.
(=+|=) :: (Source2 src1, Source src2, Monad m) => src1 m a -> src2 m a -> BasicSource m a
(=+|=) = concatSources
infixl 3 =+|=





-- | A transformation onto a sink
class Transform t where
    -- | apply transform to sink
    transformSink :: Monad m => t a b -> Sink b m r -> Sink a m r
    -- | apply transform to source
    transformSource :: (Source src, Monad m) => t a b -> src m a -> BasicSource m b
    transformSource t src = BasicSource $ transfer src . transformSink t

class (Transform t1, Transform t2, Transform tr) => TransformMerger t1 t2 tr | t1 t2 -> tr where
    -- | merges two transforms into one
    (=$=) :: t1 a b -> t2 b c -> tr a c

-- | apply a transform to a sink
(=$) :: (Transform t, Monad m) => t a b -> Sink b m r -> Sink a m r
(=$)  = transformSink
infixl 2 =$

-- | apply a transform to a source
($=) :: (Transform t, Source src, Monad m) => src m a -> t a b -> BasicSource m b
($=) = flip transformSource
infixl 1 $=


-- | Transformation that create 0..n elements out of an input and may have state
data ComplexTransformer a b = TransCont (a -> ([b], ComplexTransformer a b)) [b]
                            | TransEnd [b]
instance Transform ComplexTransformer where
    transformSink = applyComplex

applyComplex :: Monad m => ComplexTransformer a b -> Sink b m r -> Sink a m r
applyComplex _           (SinkDone r) = SinkDone r
applyComplex (TransEnd es)       sink = SinkDone $ feedSinkList es sink >>= closeSink
applyComplex (TransCont tfn tfe) sink = SinkCont next end
    where next i = liftM (applyComplex trans') (feedSinkList es sink)
            where (es, trans') = tfn i
          end = feedSinkList tfe sink >>= closeSink

mergeComplex = undefined

instance TransformMerger ComplexTransformer ComplexTransformer ComplexTransformer where
    (=$=) = mergeComplex

-- | A merged application of two transforms.
data MergedTransform a b = MergedTransform (forall r m . Monad m => Sink b m r -> Sink a m r)
instance Transform MergedTransform where
    transformSink (MergedTransform f) = f

genericMerge :: (Transform t1, Transform t2) => t1 a b -> t2 b c -> MergedTransform a c
genericMerge t1 t2 = MergedTransform $ transformSink t1 . transformSink t2

