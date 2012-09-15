module Data.SouSiT.Handle (
    -- * Source
    hSource,
    hSource',
    hSourceRes,
    hSourceNoEOF,
    hSourceNoEOF',
    hSourceResNoEOF,
    -- * Sink
    hSink,
    hSinkRes
) where

import Data.SouSiT.Source
import Data.SouSiT.Sink
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource


-- | Source from a handle. The handle will not be closed and is read till hIsEOF.
hSource :: MonadIO m => (Handle -> m a) -> Handle -> FeedSource m a
hSource get = actionSource . toEof get

-- | Same as hSource, but opens the handle when transfer is called and closes it when
--   transfer/feedToSink completes.
--   Uses 'bracket' to ensure safe release of the allocated resources.
hSource' :: (Handle -> IO a) -> IO Handle -> FeedSource IO a
hSource' get open = bracketActionSource open (liftIO . hClose) (toEof get)

-- | Same as hSource, but opens the handle when transfer is called and closes it when
--   transfer/feedToSink completes.
hSourceRes :: (MonadIO m, MonadResource m) => (Handle -> m a) -> IO Handle -> FeedSource m a
hSourceRes get open = FeedSource fun
    where fun sink = do (r,h) <- allocate open (liftIO . hClose)
                        sink' <- feedToSink (actionSource $ toEof get h) sink
                        release r
                        return sink'

toEof get h = (liftIO . hIsEOF) h >>= next
    where next True  = return Nothing
          next False = liftM Just (get h)


-- | Same as hSource, but does not check for hIsEOF and therefore never terminates.
hSourceNoEOF :: MonadIO m => (Handle -> m a) -> Handle -> FeedSource m a
hSourceNoEOF get = actionSource . liftM Just . get

-- | Same as hSource', but does not check for hIsEOF and therefore never terminates.
hSourceNoEOF' :: (Handle -> IO a) -> IO Handle -> FeedSource IO a
hSourceNoEOF' get open = bracketActionSource open hClose (liftM Just . get)

-- | Same as hSourceRes', but does not check for hIsEOF and therefore never terminates.
hSourceResNoEOF :: (MonadIO m, MonadResource m) => (Handle -> m a) -> IO Handle -> FeedSource m a
hSourceResNoEOF get open = FeedSource fun
    where fun sink = do (r,h) <- allocate open (liftIO . hClose)
                        sink' <- feedToSink (actionSource $ liftM Just $ get h) sink
                        release r
                        return sink'


-- | Sink backed by a handle. The data will be written by the provided function.
--   The sink will never change to the SinkDone state (if the device is full then
--   the operation will simply fail).
--   The handle is not closed and exceptions are not catched.
hSink :: MonadIO m => (Handle -> a -> m ()) -> Handle -> Sink a m ()
hSink put h = actionSink (put h)

-- | Same as hSink, but does opens the handle when the first item is written.
--   The handle will be closed when the sink is closed.
hSinkRes :: (MonadIO m, MonadResource m) => (Handle -> a -> m ()) -> IO Handle -> Sink a m ()
hSinkRes put open = openCloseActionSink o (release . fst) (put . snd)
    where o = allocate open (liftIO . hClose)
