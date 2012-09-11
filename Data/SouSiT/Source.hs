{-# LANGUAGE RankNTypes, BangPatterns #-}

module Data.SouSiT.Source (
    Source,
    Source2,
    transfer,
    BasicSource(..),
    BasicSource2(..),
    feedToSink,
    ($$),
    -- * utility functions
    concatSources,
    (=+=),
    (=+|=),
    -- * source construction
    actionSource,
--    bracketActionSource,
) where

import Data.SouSiT.Sink
import Control.Exception (bracket)

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


-- | Source that executes a monadic action to get its inputs. Terminates when the sink terminates
--   or the action returns Nothing.
actionSource :: Monad m => m (Maybe i) -> BasicSource2 m i
actionSource f = BasicSource2 (handleActionSource f)

handleActionSource :: Monad m => m (Maybe i) -> Sink i m r -> m (Sink i m r)
handleActionSource f !sink = sinkStatus sink >>= handleStatus
    where handleStatus (Done _)    = return sink
          handleStatus (Cont nf _) = f >>= handleInput nf
          handleInput nf Nothing  = return sink
          handleInput nf (Just i) = handleActionSource f (nf i)


{-

-- | Source that first opens a resource, then transfers itself to the sink and the closes the
--   resource again (in a bracket).
bracketActionSource :: IO a -> (a -> IO ()) -> (a -> IO (Maybe i)) -> BasicSource2 IO i
bracketActionSource open close f = BasicSource2 handle
    where handle sink = bracket open close step
            where step a = handleActionSource (f a) sink

handleActionSource :: Monad m => m (Maybe i) -> Sink i m r -> m (Sink i m r)
handleActionSource f sink = do s <- sinkStatus sink
                               i <- f
                               step s i
    where step (Cont nf _) (Just i) = nf i
          step _ _ = return sink
-}