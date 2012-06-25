{-# LANGUAGE RankNTypes, GADTs, NoMonoLocalBinds #-}

module Data.SouSiT (
    -- * Sink
    Sink(..),
    closeSink,
    feedSink,
    feedSinkList,
    decorateSink,
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
    decorateSource,
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

-- | Decorates a Sink with a monadic function. Can be used to produce debug output and such.
decorateSink df (SinkCont next done) = SinkCont next' done
    where next' i = liftM (decorateSink df) (df i >> next i)
decorateSink df (SinkDone done) = SinkDone done

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

-- | Decorates a Source with a monadic function. Can be used to produce debug output and such.
decorateSource df src = BasicSource step
    where step sink = transfer src (decorateSink df sink)

-- | Concatenates two sources.
(=+=) :: (Source2 src1, Source2 src2, Monad m) => src1 m a -> src2 m a -> BasicSource2 m a
(=+=) = concatSources2
infixl 3 =+=

-- | Concatenates two sources.
(=+|=) :: (Source2 src1, Source src2, Monad m) => src1 m a -> src2 m a -> BasicSource m a
(=+|=) = concatSources
infixl 3 =+|=






-- | A transformation onto a sink
data Transform a b where
    IdentTransform      :: Transform a a
    MappingTransform :: (a -> b) -> Transform a b
    ContTransform       :: (a -> ([b], Transform a b)) -> [b] -> Transform a b
    EndTransform        :: [b] -> Transform a b

instance C.Category Transform where
    id  = MappingTransform id
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
transformSource IdentTransform src = BasicSource $ transfer src
transformSource t src = BasicSource $ transfer src . transformSink t

-- | Apply a transform to a sink
transformSink :: Monad m => Transform a b -> Sink b m r -> Sink a m r
transformSink IdentTransform sink = sink
transformSink _ (SinkDone r) = SinkDone r
transformSink (MappingTransform f) s = step s
    where step (SinkDone r) = SinkDone r
          step (SinkCont next r) = SinkCont next' r
            where next' = liftM step . next . f
transformSink (ContTransform tfn tfe) sink = SinkCont next end
    where next i = liftM (transformSink trans') (feedSinkList es sink)
            where (es, trans') = tfn i
          end = feedSinkList tfe sink >>= closeSink
transformSink (EndTransform es) sink = SinkDone $ feedSinkList es sink >>= closeSink

-- | merges two transforms into one
mergeTransform :: Transform a b -> Transform b c -> Transform a c
mergeTransform IdentTransform t = t
mergeTransform t IdentTransform = t
mergeTransform (MappingTransform f1) (MappingTransform f2) = MappingTransform (f2 . f1)
mergeTransform _ (EndTransform r) = EndTransform r
mergeTransform (EndTransform r) t2 = EndTransform $ endTransform $ feedTransform r t2
mergeTransform (ContTransform f d) t2 = ContTransform next' done'
    where next' i = (bs, mergeTransform t1' t2')
            where (as, t1') = f i
                  (bs, t2') = feedTransform as t2
          done' = endTransform $ feedTransform d t2
mergeTransform t1 t2 = mergeTransform (contEndOnly t1) (contEndOnly t2)

-- | Converts all transformers to either ContTransform or EndTransform
contEndOnly t@(MappingTransform f) = ContTransform (\i -> ([f i], t)) []
contEndOnly IdentTransform = ContTransform (\i -> ([i], IdentTransform)) []
contEndOnly t@(ContTransform _ _) = t
contEndOnly t@(EndTransform _) = t

mapFst f (a,b) = (f a, b)

endTransform (bs, t) = bs ++ closeTransform t

closeTransform (EndTransform bs)    = bs
closeTransform (ContTransform _ bs) = bs
closeTransform _ = []

feedTransform :: [a] -> Transform a b -> ([b], Transform a b)
feedTransform es t = step [] es t
    where step outs []     t = (outs, t)
          step outs es   t@(MappingTransform f) = (outs ++ map f es, t)
          step outs (e:es) (ContTransform f _)  = let (r, t') = f e in step (outs ++ r) es t'
          step outs rest t@(EndTransform _) = (outs, t) --'rest' is lost



