module Data.SouSiT.Serial (
    serialPortSource,
    serialPortSink
) where

import Data.SouSiT
import qualified Data.SouSiT.Trans as T
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad
import Control.Concurrent
import System.Hardware.Serialport


emptyDelay = 100000 -- us => 0.1s

-- | Source from a serial port. Closing the source does not close the serial port.
serialPortSource :: SerialPort ->
     Int -> -- ^ Maximum number of bytes read at once.
     BasicSource2 IO ByteString
serialPortSource port maxread = BasicSource2 step
    where step s@(SinkCont n d) = recv port maxread >>= handle >>= step
                where handle i | nonempty i = n i
                               | otherwise  = threadDelay emptyDelay >> step s
          step done = return done
          nonempty i = BS.length i > 0

-- | Sink from a serial port. The sink is never done and does not close the serial port.
serialPortSink :: SerialPort -> 
    Bool -> -- ^ Set to True to force a flush after each write.
    Sink ByteString IO ()
serialPortSink port forceflush = SinkCont step done
    where step i = send port i >> fl >> return (SinkCont step done)
          done = return ()
          fl = if forceflush then flush port else return ()
