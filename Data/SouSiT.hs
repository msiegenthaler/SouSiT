{-# LANGUAGE RankNTypes #-}

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
    transformSink,
    transformSource,
    mergeTransform,
    (=$=),
    (=$),
    ($=)
) where

import Data.Monoid
import Control.Monad
import Control.Applicative
import qualified Control.Category as C

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
data Transform a b = MappingFunTransform (a -> b)
                   | MappingTransform    (a -> ( b,  Transform a b))
                   | ContTransform       (a -> ([b], Transform a b)) [b]
                   | EndTransform        [b]

instance C.Category Transform where
    id  = MappingFunTransform id
    (.) = flip mergeTransform

-- | apply a transform to a sink
(=$) :: (Monad m) => Transform a b -> Sink b m r -> Sink a m r
(=$)  = transformSink
infixl 2 =$

-- | apply a transform to a source
($=) :: (Source src, Monad m) => src m a -> Transform a b -> BasicSource m b
($=) = flip transformSource
infixl 1 $=

-- | merges two transforms into one
(=$=) = mergeTransform

-- | apply transform to source
transformSource :: (Source src, Monad m) => Transform a b -> src m a -> BasicSource m b
transformSource t src = BasicSource $ transfer src . transformSink t

-- | Apply a transform to a sink
transformSink :: Monad m => Transform a b -> Sink b m r -> Sink a m r
transformSink _ (SinkDone r) = SinkDone r
transformSink (MappingFunTransform f) s = step s
    where step (SinkDone r) = SinkDone r
          step (SinkCont next r) = SinkCont next' r
            where next' = liftM step . next . f
transformSink (MappingTransform f) (SinkCont next r) = SinkCont next' r
    where next' i = let (i', t') = f i in
            liftM (transformSink t') (next i')
transformSink (ContTransform tfn tfe) sink = SinkCont next end
    where next i = liftM (transformSink trans') (feedSinkList es sink)
            where (es, trans') = tfn i
          end = feedSinkList tfe sink >>= closeSink
transformSink (EndTransform es) sink = SinkDone $ feedSinkList es sink >>= closeSink

-- | merges two transforms into one
mergeTransform :: Transform a b -> Transform b c -> Transform a c
mergeTransform (MappingFunTransform f1) (MappingFunTransform f2) = MappingFunTransform (f2 . f1)
mergeTransform _ (EndTransform r) = EndTransform r
mergeTransform (EndTransform r) t2 = EndTransform $ endTransform $ feedTransform r t2
mergeTransform (ContTransform f d) t2 = ContTransform next' done'
    where next' i = (bs, mergeTransform t1' t2')
            where (as, t1') = f i
                  (bs, t2') = feedTransform as t2
          done' = endTransform $ feedTransform d t2
mergeTransform t1 t2 = mergeTransform (contEndOnly t1) (contEndOnly t2)

contEndOnly t@(MappingFunTransform f) = ContTransform (\i -> ([f i], t)) []
contEndOnly t@(MappingTransform f) = ContTransform (mapFst (:[]) . f) []
contEndOnly other = other

mapFst f (a,b) = (f a, b)

endTransform (bs, t) = bs ++ closeTransform t

closeTransform (EndTransform bs)    = bs
closeTransform (ContTransform _ bs) = bs
closeTransform _ = []

feedTransform :: [a] -> Transform a b -> ([b], Transform a b)
feedTransform es t = step [] es t
    where step outs []     t = (outs, t)
          step outs es   t@(MappingFunTransform f) = (outs ++ map f es, t)
          step outs (e:es) (MappingTransform f) = let (r, t') = f e in step (outs ++ [r]) es t'
          step outs (e:es) (ContTransform f _)  = let (r, t') = f e in step (outs ++ r) es t'
          step outs rest t@(EndTransform _) = (outs, t) --'rest' is lost



