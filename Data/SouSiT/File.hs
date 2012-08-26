{-# LANGUAGE RankNTypes #-}

module Data.SouSiT.File (
    -- * Sources
    HSource,
    fileSourceChar,
    fileSourceLine,
    fileSourceByteString,
    fileSourceWord8,
    -- * Sink
    HSink,
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



fileSourceB :: (Handle -> IO a) -> FilePath -> HSource a
fileSourceB get path = hSource' get (openBinaryFile path ReadMode)

fileSourceT :: (Handle -> IO a) -> FilePath -> HSource a
fileSourceT get path = hSource' get (openFile path ReadMode)


-- | Creates a Source2 for the file read as characters.
fileSourceChar :: FilePath -> HSource Char
fileSourceChar = fileSourceT hGetChar

-- | Creates a Source2 for the file read linewise as string
fileSourceLine :: FilePath -> HSource String
fileSourceLine = fileSourceT hGetLine

-- | Creates a Source2 for file read as ByteStrings (hGetSome).
fileSourceByteString :: Int -> FilePath -> HSource BS.ByteString
fileSourceByteString chunk = fileSourceB rd
    where rd h = BS.hGetSome h chunk

-- | Creates a Source2 for file read as single bytes (buffered).
fileSourceWord8 :: FilePath -> BasicSource IO Word8
fileSourceWord8 path = fileSourceByteString word8ChunkSize path $= T.map BS.unpack =$= T.disperse

word8ChunkSize = 256



fileSinkT :: (Handle -> a -> IO ()) -> FilePath -> HSink a
fileSinkT put path = hSink' put $ openFile path WriteMode

fileSinkB :: (Handle -> a -> IO ()) -> FilePath -> HSink a
fileSinkB put path = hSink' put $ openBinaryFile path WriteMode

-- | Creates a sink that writes the Chars into the specified file.
fileSinkChar :: FilePath -> HSink Char
fileSinkChar = fileSinkT hPutChar

-- | Creates a sink that writes the input into the file (without adding newlines).
fileSinkString :: FilePath -> HSink String
fileSinkString = fileSinkT hPutStr

-- | Creates a sink that writes each input as a line into the file.
fileSinkLine :: FilePath -> HSink String
fileSinkLine = fileSinkT hPutStrLn

-- | Creates a sink that writes the ByteStrings into the file.
fileSinkByteString :: FilePath -> HSink BS.ByteString
fileSinkByteString = fileSinkB BS.hPut

-- | Creates an unbuffered sink for writing bytes into a file.
fileSinkWord8Unbuffered :: FilePath -> HSink Word8
fileSinkWord8Unbuffered path = T.map BS.singleton =$ fileSinkByteString path

-- | Creates a sink for writing bytes into a file. The first parameter is the size of the buffer.
fileSinkWord8 :: Int -> FilePath -> HSink Word8
fileSinkWord8 bs path = T.buffer bs BS.empty BS.snoc =$ fileSinkByteString path
