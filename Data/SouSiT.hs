{-# LANGUAGE RankNTypes, GADTs, NoMonoLocalBinds #-}

module Data.SouSiT (
    -- * Sink
    Sink(..),
    SinkStatus(..),
    closeSink,
    -- ** monadic
    input,
    skip,
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
    -- decorateSource,
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

--- | Sink for data. Aggregates data to produce a single (monadic) result.
data Sink i m r = Sink { sinkStatus :: m (SinkStatus i m r) }

data SinkStatus i m r = Cont (i -> m (Sink i m r)) (m r)
                      | Done (m r)


instance Monad m => Functor (Sink i m) where
    fmap f (Sink st) = Sink (liftM mp st)
        where mp (Done r)  = Done (liftM f r)
              mp (Cont nf cf) = Cont (liftM (fmap f) . nf) (liftM f cf)

instance Monad m => Monad (Sink i m) where
    return a = let v = return a in Sink $ return $ Done v
    (Sink st) >>= f = Sink (st >>= mp)
        where mp (Done r) = liftM f r >>= sinkStatus
              mp (Cont nf cf) = return $ Cont (liftM (>>= f) . nf) noResult

instance Monad m => Applicative (Sink i m) where
    pure = return
    af <*> s = do f <- af
                  v <- s
                  return (f v)

noResult :: Monad m => m a
noResult = fail "no result: not enough input"


-- | Closes the sink and returns its result.
closeSink :: Monad m => Sink i m r -> m r
closeSink (Sink st) = st >>= handle
    where handle (Done r) = r
          handle (Cont _ r) = r


-- | Reads a value.
input :: Monad m => Sink a m a
input = Sink (return $ Cont f noResult)
    where f i = let r = return i in 
            return $ Sink (return $ Done r)

-- | Skips n input values.
skip 0 = return ()
skip i = input >> skip (i-1)



-- | Concatenates two sinks that produce a monoid.
(=||=) :: (Monad m, Monoid r) => Sink a m r -> Sink a m r -> Sink a m r
(=||=) = appendSink
infixl 3 =||=

-- | Concatenates two sinks that produce a monoid.
appendSink :: (Monad m, Monoid r) => Sink a m r -> Sink a m r -> Sink a m r
appendSink s1 s2 = do r1 <- s1
                      r2 <- s2
                      return $ mappend r1 r2







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
--decorateSource df src = BasicSource step
    --where step sink = transfer src (decorateSink df sink)

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
transformSink = undefined
{-
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
-}

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
