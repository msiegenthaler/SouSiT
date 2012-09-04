module Data.SouSiT.List (
	-- * Source
	listSource,
	-- * Sink
	listSink
) where

import Data.SouSiT.Source
import Data.SouSiT.Sink

-- | A source containing the elements of the list
listSource :: Monad m => [a] -> BasicSource2 m a
listSource l = BasicSource2 $ feedList l


-- | A sink that collects all input into a list. Does never say SinkDone.
listSink :: Monad m => Sink a m [a]
listSink = step []
    where step xs = contSink (return . step  . (xs ++) . return) (return xs)
