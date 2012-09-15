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



fileSourceB :: (Handle -> IO a) -> FilePath -> BasicSource2 IO a
fileSourceB get path = hSource' get (openBinaryFile path ReadMode)

fileSourceT :: (Handle -> IO a) -> FilePath -> BasicSource2 IO a
fileSourceT get path = hSource' get (openFile path ReadMode)


-- | Creates a Source2 for the file read as characters.
fileSourceChar :: FilePath -> BasicSource2 IO Char
fileSourceChar = fileSourceT hGetChar

-- | Creates a Source2 for the file read linewise as string
fileSourceLine :: FilePath -> BasicSource2 IO String
fileSourceLine = fileSourceT hGetLine

-- | Creates a Source2 for file read as ByteStrings (hGetSome).
fileSourceByteString :: Int -> FilePath -> BasicSource2 IO BS.ByteString
fileSourceByteString chunk = fileSourceB rd
    where rd h = BS.hGetSome h chunk

-- | Creates a Source2 for file read as single bytes (buffered).
fileSourceWord8 :: FilePath -> BasicSource IO Word8
fileSourceWord8 path = fileSourceByteString word8ChunkSize path $= T.map BS.unpack =$= T.disperse

word8ChunkSize = 256



fileSinkT :: (Handle -> a -> IO ()) -> FilePath -> Sink a IO ()
fileSinkT put path = hSink' put $ openFile path WriteMode

fileSinkB :: (Handle -> a -> IO ()) -> FilePath -> Sink a IO ()
fileSinkB put path = hSink' put $ openBinaryFile path WriteMode

-- | Creates a sink that writes the Chars into the specified file.
fileSinkChar :: FilePath -> Sink Char IO ()
fileSinkChar = fileSinkT hPutChar

-- | Creates a sink that writes the input into the file (without adding newlines).
fileSinkString :: FilePath -> Sink String IO ()
fileSinkString = fileSinkT hPutStr

-- | Creates a sink that writes each input as a line into the file.
fileSinkLine :: FilePath -> Sink String IO ()
fileSinkLine = fileSinkT hPutStrLn

-- | Creates a sink that writes the ByteStrings into the file.
fileSinkByteString :: FilePath -> Sink BS.ByteString IO ()
fileSinkByteString = fileSinkB BS.hPut

-- | Creates an unbuffered sink for writing bytes into a file.
fileSinkWord8Unbuffered :: FilePath -> Sink Word8 IO ()
fileSinkWord8Unbuffered path = T.map BS.singleton =$ fileSinkByteString path

-- | Creates a sink for writing bytes into a file. The first parameter is the size of the buffer.
fileSinkWord8 :: Int -> FilePath -> Sink Word8 IO ()
fileSinkWord8 bs path = T.buffer bs BS.empty BS.snoc =$ fileSinkByteString path
