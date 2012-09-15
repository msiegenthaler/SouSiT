{-# LANGUAGE RankNTypes #-}

module Data.SouSiT.File (
    -- * Sources
    fileSourceChar,
    fileSourceLine,
    fileSourceByteString,
    fileSourceWord8,
    -- * Sink
    fileSinkChar,
    fileSinkString,
    fileSinkLine,
    fileSinkByteString,
    fileSinkWord8,
    fileSinkWord8Unbuffered
) where

import System.IO
import qualified Data.ByteString as BS
import Data.SouSiT
import Data.SouSiT.Handle
import qualified Data.SouSiT.Trans as T
import Data.Word
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource


fileSourceB :: (MonadIO m, MonadResource m) => (Handle -> IO a) -> FilePath -> BasicSource2 m a
fileSourceB get path = hSourceRes (liftIO. get) $ openBinaryFile path ReadMode

fileSourceT :: (MonadIO m, MonadResource m) => (Handle -> IO a) -> FilePath -> BasicSource2 m a
fileSourceT get path = hSourceRes (liftIO . get) $ openFile path ReadMode


-- | Creates a Source2 for the file read as characters.
fileSourceChar :: (MonadIO m, MonadResource m) => FilePath -> BasicSource2 m Char
fileSourceChar = fileSourceT hGetChar

-- | Creates a Source2 for the file read linewise as string
fileSourceLine :: (MonadIO m, MonadResource m) => FilePath -> BasicSource2 m String
fileSourceLine = fileSourceT hGetLine

-- | Creates a Source2 for file read as ByteStrings (hGetSome).
fileSourceByteString :: (MonadIO m, MonadResource m) => Int -> FilePath -> BasicSource2 m BS.ByteString
fileSourceByteString chunk = fileSourceB rd
    where rd h = BS.hGetSome h chunk

-- | Creates a Source2 for file read as single bytes (buffered).
fileSourceWord8 :: (MonadIO m, MonadResource m) => FilePath -> BasicSource m Word8
fileSourceWord8 path = fileSourceByteString word8ChunkSize path $= T.map BS.unpack =$= T.disperse

word8ChunkSize = 256


liftPut put h a = liftIO $ put h a

fileSinkT :: (MonadIO m, MonadResource m) => (Handle -> a -> IO ()) -> FilePath -> Sink a m ()
fileSinkT put path = hSinkRes (liftPut put) $ openFile path WriteMode

fileSinkB :: (MonadIO m, MonadResource m) => (Handle -> a -> IO ()) -> FilePath -> Sink a m ()
fileSinkB put path = hSinkRes (liftPut put) $ openBinaryFile path WriteMode

-- | Creates a sink that writes the Chars into the specified file.
fileSinkChar :: (MonadIO m, MonadResource m) => FilePath -> Sink Char m ()
fileSinkChar = fileSinkT hPutChar

-- | Creates a sink that writes the input into the file (without adding newlines).
fileSinkString :: (MonadIO m, MonadResource m) => FilePath -> Sink String m ()
fileSinkString = fileSinkT hPutStr

-- | Creates a sink that writes each input as a line into the file.
fileSinkLine :: (MonadIO m, MonadResource m) => FilePath -> Sink String m ()
fileSinkLine = fileSinkT hPutStrLn

-- | Creates a sink that writes the ByteStrings into the file.
fileSinkByteString :: (MonadIO m, MonadResource m) => FilePath -> Sink BS.ByteString m ()
fileSinkByteString = fileSinkB BS.hPut

-- | Creates an unbuffered sink for writing bytes into a file.
fileSinkWord8Unbuffered :: (MonadIO m, MonadResource m) => FilePath -> Sink Word8 m ()
fileSinkWord8Unbuffered path = T.map BS.singleton =$ fileSinkByteString path

-- | Creates a sink for writing bytes into a file. The first parameter is the size of the buffer.
fileSinkWord8 :: (MonadIO m, MonadResource m) => Int -> FilePath -> Sink Word8 m ()
fileSinkWord8 bs path = T.buffer bs BS.empty BS.snoc =$ fileSinkByteString path
