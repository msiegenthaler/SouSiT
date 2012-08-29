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


-- | A sink that executes (atomically) a STM action for every input received.
--   The sink continues as long as the action returns Nothing. When the action
--   returns Just, then that value is the result of the sink.
stmSink :: (a -> STM (Maybe r)) -> Sink a IO (Maybe r)
stmSink f = maybeSink (atomically . f)

-- | A sink that executes (atomically) a STM action for every input received.
--   The sink never terminates.
stmSink' :: (a -> STM ()) -> Sink a IO ()
stmSink' f = actionSink (atomically . f)

-- | Sink that writes all items into a TChan.
tchanSink :: TChan a -> Sink a IO ()
tchanSink chan = stmSink' (writeTChan chan)


-- | Source that executes a STM action to get a new item. When the action returns 'Nothing'
--   then the source is depleted.
stmSource :: STM (Maybe a) -> BasicSource2 IO a
stmSource f = actionSource (atomically f)

-- | Source that executes a STM action to get a new item. Does never run out of items.
stmSource' :: STM a -> BasicSource2 IO a
stmSource' f = actionSource (atomically $ liftM Just f)

-- | Source that reads from a TChan. Does never run out of items (just waits for new ones
--   written to the TChan).
tchanSource :: TChan a -> BasicSource2 IO a
tchanSource c = stmSource' (readTChan c)
