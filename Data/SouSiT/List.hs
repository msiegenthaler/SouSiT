module Data.SouSiT.List (
	-- * Source
	listSource,
	-- * Sink
	listSink
) where

import Data.SouSiT

-- | A source containing the elements of the list
listSource :: Monad m => [a] -> BasicSource2 m a
listSource l = BasicSource2 $ feedSinkList l


-- | A sink that collects all input into a list. Does never say SinkDone.
listSink :: Monad m => Sink a m [a]
listSink = listSink' []
listSink' xs = SinkCont next (return xs)
	where next x = return $ listSink' (xs ++ [x])
