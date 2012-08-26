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
import Control.Monad


-- | Source for file IO-handle operations
type HSource a = BasicSource2 IO a

-- | Source from a handle. The handle will not be closed and is read till hIsEOF.
hSource :: (Handle -> IO a) -> Handle -> HSource a
hSource get = actionSource . toEof get

-- | Same as hSource, but opens the handle when transfer is called and closes it when
--   all data is read.
hSource' :: (Handle -> IO a) -> IO Handle -> HSource a
hSource' get open = bracketActionSource open hClose (toEof get)

toEof get h = hIsEOF h >>= next
    where next True  = return Nothing
          next False = liftM Just (get h)


-- | Same as hSource, but does not check for hIsEOF and therefore never terminates.
hSourceNoEOF :: (Handle -> IO a) -> Handle -> HSource a
hSourceNoEOF get = actionSource . liftM Just . get

-- | Same as hSource', but does not check for hIsEOF and therefore never terminates.
hSourceNoEOF' :: (Handle -> IO a) -> IO Handle -> HSource a
hSourceNoEOF' get open = bracketActionSource open hClose (liftM Just . get)


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
