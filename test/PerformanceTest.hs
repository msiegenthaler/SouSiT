{-# LANGUAGE Rank2Types, BangPatterns #-}
module Main (
    main
) where

import Criterion.Main
import Data.SouSiT
import Data.SouSiT.Sink
import qualified Data.SouSiT.Trans as T
import Data.SouSiT.List
import Control.Monad.Identity


countSink :: (Num n, Monad m) => Sink a m n
countSink = step 0
  where step !n = contSink f (return n)
          where f _ = step (n + 1)

firstSink :: Monad m => Sink a m a
firstSink = input

elemCountSource n = listSource [1..n]


countList :: Monad m => Int -> m Int
countList n = elemCountSource n $$ countSink

countListTrans :: Monad m => Int -> m Int
countListTrans n = elemCountSource n $$ T.count =$ firstSink


type PerfFun a = forall m . Monad m => Int -> m a

perf :: String -> Int -> PerfFun a -> Benchmark
perf label cnt f = bgroup label [
            bench "in Identity" $ whnf (runIdentity . f) cnt,
            bench "in IO"       $ io $ f cnt
        ]
    where io :: IO a -> IO a
          io = id


main = defaultMain [
            perf "count elems from listSource"          c countList,
            perf "count elems in Trans from listSource" c countListTrans
    ] where c = 100000
