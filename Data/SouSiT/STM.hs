module Data.SouSiT.STM (
    -- * Sinks
    stmSink,
    stmSink',
    tchanSink,
    -- * Sources
    stmSource,
    stmSource',
    tchanSource
) where

import Data.SouSiT.Source
import Data.SouSiT.Sink
import Control.Monad
import Control.Concurrent.STM
import Control.Monad.IO.Class


-- | A sink that executes (atomically) a STM action for every input received.
--   The sink continues as long as the action returns Nothing. When the action
--   returns Just, then that value is the result of the sink.
stmSink :: MonadIO m => (a -> STM (Maybe r)) -> Sink a m (Maybe r)
stmSink f = maybeSink (liftIO . atomically . f)

-- | A sink that executes (atomically) a STM action for every input received.
--   The sink never terminates.
stmSink' :: MonadIO m => (a -> STM ()) -> Sink a m ()
stmSink' f = actionSink (liftIO . atomically . f)

-- | Sink that writes all items into a TChan.
tchanSink :: MonadIO m => TChan a -> Sink a m ()
tchanSink chan = stmSink' (writeTChan chan)


-- | Source that executes a STM action to get a new item. When the action returns 'Nothing'
--   then the source is depleted.
stmSource :: MonadIO m => STM (Maybe a) -> FeedSource m a
stmSource f = actionSource (liftIO . atomically $ f)

-- | Source that executes a STM action to get a new item. Does never run out of items.
stmSource' :: MonadIO m => STM a -> FeedSource m a
stmSource' f = actionSource (liftIO . atomically $ liftM Just f)

-- | Source that reads from a TChan. Does never run out of items (just waits for new ones
--   written to the TChan).
tchanSource :: MonadIO m => TChan a -> FeedSource m a
tchanSource c = stmSource' (readTChan c)
