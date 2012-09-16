{-# LANGUAGE RankNTypes, BangPatterns #-}

module Data.SouSiT.Source (
    Source,
    transfer,
    SimpleSource(..),
    FeedSource(..),
    ($$),
    -- * utility functions
    concatSources,
    concatSources',
    (=+=),
    (=+|=),
    -- * source construction
    actionSource,
    bracketActionSource
) where

import Data.SouSiT.Sink
import Control.Exception (bracket)


-- | Something that produces data to be processed by a sink
class Source src where
    transfer :: Monad m => src m a -> Sink a m r -> m r

-- | Transfer the data from the source into the sink
($$) :: (Source src, Monad m) => src m a -> Sink a m r -> m r
($$) = transfer
infixl 0 $$

-- | A basic instance of Source
data SimpleSource m a = SimpleSource (forall r. Sink a m r -> m r)
instance Source SimpleSource where
    transfer (SimpleSource f) = f

-- | A basic instance of FeedSource (and Source)
data FeedSource m a = FeedSource { feedToSink :: forall r. Sink a m r -> m (Sink a m r) }
instance Source FeedSource where
    transfer src sink = feedToSink src sink >>= closeSink

-- | Concatenates two sources.
concatSources :: (Source src2, Monad m) => FeedSource m a -> src2 m a -> SimpleSource m a
concatSources src1 src2 = SimpleSource f
    where f sink = feedToSink src1 sink >>= transfer src2

-- | Concatenates two sources yielding a FeedSource.
concatSources' :: Monad m => FeedSource m a -> FeedSource m a -> FeedSource m a
concatSources' src1 src2 = FeedSource f
    where f sink = feedToSink src1 sink >>= feedToSink src2

-- | Concatenates two sources.
(=+=) :: Monad m => FeedSource m a -> FeedSource m a -> FeedSource m a
(=+=) = concatSources'
infixl 3 =+=

-- | Concatenates two sources.
(=+|=) :: (Source src2, Monad m) => FeedSource m a -> src2 m a -> SimpleSource m a
(=+|=) = concatSources
infixl 3 =+|=


-- | Source that executes a monadic action to get its inputs. Terminates when the sink terminates
--   or the action returns Nothing.
actionSource :: Monad m => m (Maybe i) -> FeedSource m i
actionSource f = FeedSource (handleActionSource f)

-- | Source that first opens a resource, then transfers itself to the sink and the closes the
--   resource again (in a bracket).
bracketActionSource :: IO a -> (a -> IO ()) -> (a -> IO (Maybe i)) -> FeedSource IO i
bracketActionSource open close f = FeedSource handle
    where handle sink = bracket open close step
            where step a = handleActionSource (f a) sink

handleActionSource :: Monad m => m (Maybe i) -> Sink i m r -> m (Sink i m r)
handleActionSource f !sink = sinkStatus sink >>= handleStatus
    where handleStatus (Done _) = return sink
          handleStatus (Cont nf _) = f >>= handleInput nf
          handleInput _  Nothing  = return sink
          handleInput nf (Just i) = nf i >>= handleActionSource f
