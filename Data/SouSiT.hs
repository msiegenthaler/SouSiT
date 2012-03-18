{-# LANGUAGE RankNTypes, TupleSections #-}

module Data.SouSiT (
    -- * Sink
    Sink(..),
    closeSink,
    concatSink,
    (=|=),
    appendSink,
    (=||=),
    -- * Source
    Source,
    ($$),
    concatSources,
    (=+=),
    (=+|=),
    BasicSource(..),
    BasicSource2(..),
    -- * Transform
    Transform(..),
    (=$=),
    (=$),
    ($=),
    ComplexTransformer(..),
    MergedTransform,
    mergeTransforms,
) where

import Data.Monoid

--- | Sink for data. Aggregates data to produce a single result (i.e. an IO).
data Sink a r = SinkCont (a -> Sink a r) r
              | SinkDone r

instance Functor (Sink a) where
    fmap = mapSink

-- | Close the sink and return its result
closeSink :: Sink a r -> r
closeSink (SinkDone r) = r
closeSink (SinkCont _ r) = r

-- | Concatenates two sinks.
(=|=) = concatSink
infixl 3 =|=

-- | Concatenates two sinks.
-- After the first returns SinkDone, the data is inputed into the second.
concatSink :: Sink a r1 -> Sink a r2 -> Sink a (r1, r2)
concatSink (SinkDone r)   s2 = mapSink (r,) s2
concatSink (SinkCont f r) s2 = SinkCont step (r, closeSink s2)
    where step i = concatSink (f i) s2

-- | Concatenates two sinks that produce a monoid.
(=||=) :: Monoid m => Sink a m -> Sink a m -> Sink a m
(=||=) = appendSink
infixl 3 =||=

-- | Concatenates two sinks that produce a monoid.
appendSink :: Monoid m => Sink a m -> Sink a m -> Sink a m
appendSink s1 s2 = fmap append $ concatSink s1 s2
    where append (r1, r2) = r1 `mappend` r2


mapSink :: (r1 -> r2) -> Sink a r1 -> Sink a r2
mapSink f (SinkCont next r) = SinkCont (mapSink f . next) (f r)
mapSink f (SinkDone r)      = SinkDone (f r)

feedSink :: Sink a r -> a -> Sink a r
feedSink (SinkCont f _) i = f i
feedSink sink _ = sink




-- | Something that produces data to be processed by a sink
class Source src where
    transfer :: src a -> Sink a r -> r

-- | An additional typeclass for more flexible sources (allowing i.e. concats)
class Source src => Source2 src where
    feedToSink :: src a -> Sink a r -> Sink a r

-- | Transfer the data from the source into the sink
($$) :: Source src => src a -> Sink a r -> r
($$) = transfer
infixl 0 $$

-- | Concatenates two sources.
concatSources :: (Source2 src1, Source src2) => src1 a -> src2 a -> BasicSource a
concatSources src1 src2 = BasicSource (transfer src2 . feedToSink src1)

-- | Concatenates two sources yielding a Source2.
concatSources2 :: (Source2 src1, Source2 src2) => src1 a -> src2 a -> BasicSource2 a
concatSources2 src1 src2 = BasicSource2 (feedToSink src2 . feedToSink src1)

-- | Concatenates two sources.
(=+=) :: (Source2 src1, Source2 src2) => src1 a -> src2 a -> BasicSource2 a
(=+=) = concatSources2
infixl 3 =+=

-- | Concatenates two sources.
(=+|=) :: (Source2 src1, Source src2) => src1 a -> src2 a -> BasicSource a
(=+|=) = concatSources
infixl 3 =+|=


-- | A source with an applied transformer
data BasicSource a = BasicSource (forall r. Sink a r -> r)
instance Source BasicSource where
    transfer (BasicSource f) = f

-- | A normal source of type Source2.
data BasicSource2 a = BasicSource2 (forall r. Sink a r -> Sink a r)
instance Source2 BasicSource2 where
    feedToSink (BasicSource2 f) = f
instance Source BasicSource2 where
    transfer src = closeSink . feedToSink src




-- | A transformation onto a sink
class Transform t where
    transformSink :: t a b -> Sink b r -> Sink a r
    transformSource :: Source src => t a b -> src a -> BasicSource b
    transformSource t src = BasicSource $ transfer src . transformSink t

-- | merges two transforms into one
(=$=) :: (Transform t1, Transform t2) => t1 a b -> t2 b c -> MergedTransform a c
(=$=) = mergeTransforms
infixl 2 =$=

-- | apply a transform to a sink
(=$) :: (Transform t) => t a b -> Sink b r -> Sink a r
(=$)  = transformSink
infixl 2 =$

-- | apply a transform to a source
($=) :: (Transform t, Source src) => src a -> t a b -> BasicSource b
($=) = flip transformSource
infixl 1 $=



-- | Transformation that create 0..n elements out of an input and may have state
data ComplexTransformer a b = TransCont (a -> ([b], ComplexTransformer a b)) [b]
                            | TransEnd [b]
instance Transform ComplexTransformer where
    transformSink = applyComplexTransformer

applyComplexTransformer :: ComplexTransformer a b -> Sink b r -> Sink a r
applyComplexTransformer _                   (SinkDone r) = SinkDone r
applyComplexTransformer (TransEnd es)       sink         = SinkDone $ closeSink $ foldSink es sink
applyComplexTransformer (TransCont tfn tfe) sink         = SinkCont next end
    where next i = applyComplexTransformer trans' $ foldSink es sink
            where (es, trans') = tfn i
          end = closeSink $ foldSink tfe sink

foldSink :: [a] -> Sink a r -> Sink a r
foldSink [] sink = sink
foldSink _ done@(SinkDone _) = done
foldSink (x:es) (SinkCont f r) = foldSink es $ f x


-- | A merged application of two transforms.
data MergedTransform a b = MergedTransform (forall r . Sink b r -> Sink a r)
instance Transform MergedTransform where
    transformSink (MergedTransform f) = f

-- | merges two transforms into one
mergeTransforms :: (Transform t1, Transform t2) => t1 a b -> t2 b c -> MergedTransform a c
mergeTransforms t1 t2 = MergedTransform $ transformSink t1 . transformSink t2




