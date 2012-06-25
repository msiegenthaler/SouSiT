module Data.SouSiT.Handle (
    -- * Source
    HSource,
    hSource,
    hSource',
    -- * Sink
    HSink,
    hSink,
    hSink'
) where

import Data.SouSiT
import System.IO


-- | Source for file IO-handle operations
type HSource a = BasicSource2 IO a

hSource :: (Handle -> IO a) -> Handle -> HSource a
hSource get h = BasicSource2 (readNext get h)

hSource' :: (Handle -> IO a) -> IO Handle -> HSource a
hSource' get open = BasicSource2 step
    where step sink = do h <- open; readNext get h sink

readNext get h sink@(SinkCont f _) = hIsEOF h >>= step
    where step True  = return sink
          step False = get h >>= f >>= readNext get h
readNext _   _ done = return done



-- | Sink for file IO-handle operations
type HSink a = Sink a IO ()

-- | Sink backed by a handle. The data will be written by the provided function.
--   The sink will never change to the SinkDone state (if the device is full then
--   the operation will simply fail).
--   The handle is not closed and exceptions are not catched.
hSink :: (Handle -> a -> IO ()) -> Handle -> HSink a
hSink put h = SinkCont step noop
    where step = runHSink put (\_ -> noop) h

-- | Same as hSink, but does open the handle only when the first item is written.
--   The handle will be closed when the sink is closed.
hSink' :: (Handle -> a -> IO ()) -> IO Handle -> HSink a
hSink' put open = SinkCont first (return ())
    where first i = do h <- open; runHSink put hClose h i

runHSink put close h i = put h i >> return (SinkCont step (close h))
    where step = runHSink put close h

noop = return ()
