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

import Control.Concurrent.STM
import Data.SouSiT


-- | A sink that executes (atomically) a STM action for every input received.
--   The sink continues as long as the action returns Nothing. When the action
--   returns Just, then that value is the result of the sink.
stmSink :: (a -> STM (Maybe r)) -> Sink a IO (Maybe r)
stmSink f = SinkCont step (return Nothing)
    where step i = atomically (f i) >>= cont
          cont (Just r) = return $ SinkDone $ return $ Just r
          cont Nothing  = return $ stmSink f

-- | A sink that executes (atomically) a STM action for every input received.
--   The sink never terminates.
stmSink' :: (a -> STM ()) -> Sink a IO ()
stmSink' f = SinkCont step (return ())
    where step i = atomically (f i) >> return (stmSink' f)

-- | Sink that writes all items into a TChan.
tchanSink :: TChan a -> Sink a IO ()
tchanSink chan = stmSink' (writeTChan chan)


-- | Source that executes a STM action to get a new item. When the action returns 'Nothing'
--   then the source is depleted.
stmSource :: STM (Maybe a) -> BasicSource2 IO a
stmSource f = BasicSource2 step
    where step (SinkCont next done) = atomically f >>= doit
            where doit (Just i) = next i >>= step
                  doIt Nothing  = done
          step done = return done

-- | Source that executes a STM action to get a new item. Does never run out of items.
stmSource' :: STM a -> BasicSource2 IO a
stmSource' f = BasicSource2 step
    where step (SinkCont next _) = atomically f >>= next >>= step
          step done = return done

-- | Source that reads from a TChan. Does never run out of items (just waits for new ones
--   written to the TChan).
tchanSource :: TChan a -> BasicSource2 IO a
tchanSource c = stmSource' (readTChan c)
