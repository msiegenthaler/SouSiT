module Data.SouSiT.List (
	-- * Source
	listSource,
	-- * Sink
	listSink
) where

import Data.SouSiT.Source
import Data.SouSiT.Sink

-- | A source containing the elements of the list
listSource :: Monad m => [a] -> FeedSource m a
listSource l = FeedSource $ return . feedList l


-- | A sink that collects all input into a list. Does never say SinkDone.
listSink :: Monad m => Sink a m [a]
listSink = step []
    where step xs = contSink' (step . (xs ++) . return) (return xs)
