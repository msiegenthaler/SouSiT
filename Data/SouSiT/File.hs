{-# LANGUAGE RankNTypes #-}

module Data.SouSiT.File (
    -- * Sources
    IOSource,
    fileSourceChar,
    fileSourceLine,
    fileSourceByteString,
    fileSourceWord8,
    -- * Sink
    IOSink,
    fileSinkChar,
    fileSinkString,
    fileSinkLine,
    fileSinkByteString
) where

import System.IO
import Control.Monad
import Control.Applicative
import qualified Data.ByteString as BS
import Data.SouSiT
import Data.Word


-- | Source for file IO operations
type IOSource a = BasicSource2 IO a

fileSourceT :: (forall r. Sink a IO r -> Handle -> IO (Sink a IO r)) -> FilePath -> IOSource a
fileSourceT handler path = BasicSource2 $ withFile path ReadMode . handler

fileSourceB :: (forall r. Sink a IO r -> Handle -> IO (Sink a IO r)) -> FilePath -> IOSource a
fileSourceB handler path = BasicSource2 $ withBinaryFile path ReadMode . handler

readNext :: (Handle -> IO a) -> Sink a IO r -> Handle -> IO (Sink a IO r)
readNext rd sink@(SinkCont f _) h = do
    eof <- hIsEOF h
    if eof then return sink
           else do e <- rd h
                   next <- f e
                   readNext rd next h
readNext _ done h = return done


-- | Creates a Source2 for the file read as characters.
fileSourceChar :: FilePath -> IOSource Char
fileSourceChar = fileSourceT $ readNext hGetChar

-- | Creates a Source2 for the file read linewise as string
fileSourceLine :: FilePath -> IOSource String
fileSourceLine = fileSourceT $ readNext hGetLine

-- | Creates a Source2 for file read as ByteStrings (hGetSome).
fileSourceByteString :: Int -> FilePath -> IOSource BS.ByteString
fileSourceByteString chunk = fileSourceB $ readNext rd
    where rd h = BS.hGetSome h chunk

-- | Creates a Source2 for file read as single bytes (buffered).
fileSourceWord8 :: FilePath -> IOSource Word8
fileSourceWord8 = fileSourceB readWord8

readWord8 :: Sink Word8 IO r -> Handle -> IO (Sink Word8 IO r)
readWord8 sink@(SinkCont f _) h = do
    eof <- hIsEOF h
    if eof then return sink
           else do chunk <- BS.hGetSome h word8ChunkSize
                   BS.foldl feedM (return sink) chunk
readWord8 done h = return done

feedM :: (Monad m, Applicative m) => m (Sink a m r) -> a -> m (Sink a m r)
feedM ms i = ms >>= flip feedSink i

word8ChunkSize = 256



-- | Sink for file IO operations
type IOSink a = Sink a IO ()

ioSink :: (IO Handle) -> (Handle -> a -> IO ()) -> IOSink a
ioSink open put = SinkCont first noop
    where first i = open >>= flip (runIOSink put) i
          noop = return ()

runIOSink :: (Handle -> a -> IO ()) -> Handle -> a -> IO (IOSink a)
runIOSink put h i = put h i >> return next
    where next = SinkCont (runIOSink put h) (hClose h)

fileSinkT :: (Handle -> a -> IO ()) -> FilePath -> IOSink a
fileSinkT put path = ioSink open put
    where open = openFile path WriteMode

fileSinkB :: (Handle -> a -> IO ()) -> FilePath -> IOSink a
fileSinkB put path = ioSink open put
    where open = openBinaryFile path WriteMode

-- | Creates a sink that writes the Chars into the specified file.
fileSinkChar :: FilePath -> IOSink Char
fileSinkChar = fileSinkT hPutChar

-- | Creates a sink that writes the input into the file (without adding newlines).
fileSinkString :: FilePath -> IOSink String
fileSinkString = fileSinkT hPutStr

-- | Creates a sink that writes each input as a line into the file.
fileSinkLine :: FilePath -> IOSink String
fileSinkLine = fileSinkT hPutStrLn

-- | Creates a sink that writes the ByteStrings into the file.
fileSinkByteString :: FilePath -> IOSink BS.ByteString
fileSinkByteString = fileSinkB BS.hPut






