module Data.SouSiT.List (
	-- * Source
	listSource,
	-- * Sink
	listSink
) where

import Data.SouSiT

-- | A source containing the elements of the list
listSource :: [a] -> BasicSource2 a
listSource l = BasicSource2 $ feedList2Sink l

feedList2Sink :: [a] -> Sink a r -> Sink a r
feedList2Sink [] sink = sink
feedList2Sink _ done@(SinkDone _) = done
feedList2Sink (x:es) (SinkCont f r) = feedList2Sink es $ f x


-- | A sink that collects all input into a list. Does never say SinkDone.
listSink :: Sink a [a]
listSink = listSink' []
listSink' es = SinkCont app es
    where app e = listSink' (es ++ [e])
