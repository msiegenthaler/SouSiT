module Data.SouSiT.Handle (
    -- * Source
    hSource,
    hSource',
    hSourceNoEOF,
    hSourceNoEOF',
    -- * Sink
    hSink,
    hSink'
) where

import Data.SouSiT.Source
import Data.SouSiT.Sink
import System.IO
import Control.Monad
import Control.Monad.IO.Class


-- | Source from a handle. The handle will not be closed and is read till hIsEOF.
hSource :: MonadIO m => (Handle -> m a) -> Handle -> BasicSource2 m a
hSource get = actionSource . toEof get

-- | Same as hSource, but opens the handle when transfer is called and closes it when
--   all data is read.
hSource' :: (Handle -> IO a) -> IO Handle -> BasicSource2 IO a
hSource' get open = bracketActionSource open (liftIO . hClose) (toEof get)

toEof get h = (liftIO . hIsEOF) h >>= next
    where next True  = return Nothing
          next False = liftM Just (get h)


-- | Same as hSource, but does not check for hIsEOF and therefore never terminates.
hSourceNoEOF :: MonadIO m => (Handle -> m a) -> Handle -> BasicSource2 m a
hSourceNoEOF get = actionSource . liftM Just . get

-- | Same as hSource', but does not check for hIsEOF and therefore never terminates.
hSourceNoEOF' :: (Handle -> IO a) -> IO Handle -> BasicSource2 IO a
hSourceNoEOF' get open = bracketActionSource open hClose (liftM Just . get)


-- | Sink backed by a handle. The data will be written by the provided function.
--   The sink will never change to the SinkDone state (if the device is full then
--   the operation will simply fail).
--   The handle is not closed and exceptions are not catched.
hSink :: MonadIO m => (Handle -> a -> m ()) -> Handle -> Sink a m ()
hSink put h = actionSink (put h)

-- | Same as hSink, but does open the handle only when the first item is written.
--   The handle will be closed when the sink is closed.
hSink' :: MonadIO m => (Handle -> a -> m ()) -> m Handle -> Sink a m ()
hSink' put open = openCloseActionSink open (liftIO . hClose) put
