{-# LANGUAGE RankNTypes #-}

module Data.SouSit.File (
    -- * Sources
    fileSourceChar,
    fileSourceLine,
    fileSourceByteString,
    fileSourceWord8,
    -- * Sink
    fileSinkChar
) where

import Data.Word
import Data.SouSiT
import System.IO
import qualified Data.ByteString as BS


--TODO delete
import Debug.Trace
import Data.SouSiT.List
import Data.SouSiT.List as T

fileSource :: (forall r. Sink a r -> Handle -> IO (Sink a r)) -> FilePath -> BasicSource2 IO a
fileSource handler path = BasicSource2 $ withFile path ReadMode . handler

fileSourceB :: (forall r. Sink a r -> Handle -> IO (Sink a r)) -> FilePath -> BasicSource2 IO a
fileSourceB handler path = BasicSource2 $ withBinaryFile path ReadMode . handler


readNext :: (Handle -> IO a) -> Sink a r -> Handle -> IO (Sink a r)
readNext rd sink@(SinkCont f _) h = do
    eof <- hIsEOF h
    if eof then return sink
           else do c <- rd h
                   readNext rd (f c) h
readNext _ done h = return done


-- | Creates a Source2 for the file read as characters.
fileSourceChar :: FilePath -> BasicSource2 IO Char
fileSourceChar = fileSource $ readNext hGetChar

-- | Creates a Source2 for the file read linewise as string
fileSourceLine :: FilePath -> BasicSource2 IO String
fileSourceLine = fileSource $ readNext hGetLine

-- | Creates a Source2 for file read as ByteStrings (hGetSome).
fileSourceByteString :: Int -> FilePath -> BasicSource2 IO BS.ByteString
fileSourceByteString chunk = fileSourceB $ readNext rd
    where rd h = BS.hGetSome h chunk

-- | Creates a Source2 for file read as single bytes.
fileSourceWord8 :: FilePath -> BasicSource2 IO Word8
fileSourceWord8 = fileSourceB readWord8

readWord8 :: Sink Word8 r -> Handle -> IO (Sink Word8 r)
readWord8 sink@(SinkCont f _) h = do
    eof <- hIsEOF h
    if eof then return sink
           else do chunk <- BS.hGetSome h word8ChunkSize
                   return $ BS.foldl feedSink sink chunk
readWord8 done h = return done

word8ChunkSize = 256



-- | Creates a sink that writes the Chars into the specified file.
fileSinkChar :: FilePath -> Sink Char (IO ())
fileSinkChar path = trace ("fileSinkFor " ++ path) $ fileSinkChar' path
fileSinkChar' path = SinkCont (fs2' open) $ return ()
    where open = openFile path WriteMode


fs2' = trace "call to fs2" fs2
fs2 :: IO Handle -> Char -> Sink Char (IO ())
fs2 prev i = SinkCont (fs2 action) (action >>= hClose)
    where action = do h <- prev
                      print $ "Saving " ++ [i] --TODO
                      hPutChar h i
                      return h


--copy :: FilePath -> FilePath -> ()
copy p1 p2 = fileSourceChar p1 $$ fileSinkChar p2