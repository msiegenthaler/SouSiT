module Data.SouSiT.Handle (
    -- * Source
    HSource,
    hSource,
    hSource',
    hSourceNoEOF,
    hSourceNoEOF',
    -- * Sink
    HSink,
    hSink,
    hSink'
) where

import Data.SouSiT
import System.IO


-- | Source for file IO-handle operations
type HSource a = BasicSource2 IO a


-- | Source from a handle. The handle will not be closed and is read till hIsEOF.
hSource :: (Handle -> IO a) -> Handle -> HSource a
hSource get h = BasicSource2 (readAll get h)

-- | Same as hSource, but opens the handle when transfer is called and closes it when
--   all data is read.
hSource' :: (Handle -> IO a) -> IO Handle -> HSource a
hSource' get open = BasicSource2 step
    where step sink = do h <- open; readAll get h sink

readAll get h sink = do status <- sinkStatus sink
                        eof <- hIsEOF h
                        step status eof
    where step (Cont f _) False = get h >>= f >>= readAll get h
          step _ _ = return sink


-- | Same as hSource, but does not check for hIsEOF and therefore never terminates.
hSourceNoEOF :: (Handle -> IO a) -> Handle -> HSource a
hSourceNoEOF get h = BasicSource2 (readAllNoEOF get h)

-- | Same as hSource', but does not check for hIsEOF and therefore never terminates.
hSourceNoEOF' :: (Handle -> IO a) -> IO Handle -> HSource a
hSourceNoEOF' get open = BasicSource2 step
    where step sink = do h <- open; readAllNoEOF get h sink

readAllNoEOF get h sink = sinkStatus sink >>= step
    where step (Done r) = return sink
          step (Cont f _) = get h >>= f >>= readAllNoEOF get h


-- | Sink for file IO-handle operations
type HSink a = Sink a IO ()

-- | Sink backed by a handle. The data will be written by the provided function.
--   The sink will never change to the SinkDone state (if the device is full then
--   the operation will simply fail).
--   The handle is not closed and exceptions are not catched.
hSink :: (Handle -> a -> IO ()) -> Handle -> HSink a
hSink put h = actionSink (put h)

-- | Same as hSink, but does open the handle only when the first item is written.
--   The handle will be closed when the sink is closed.
hSink' :: (Handle -> a -> IO ()) -> IO Handle -> HSink a
hSink' put open = openCloseActionSink open hClose put
