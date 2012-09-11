{-# LANGUAGE BangPatterns #-}
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

io :: IO a -> IO a
io = id

pure = runIdentity

countList :: Monad m => Int -> m Int
countList n = elemCountSource n $$ countSink

countListTrans :: Monad m => Int -> m Int
countListTrans n = elemCountSource n $$ T.count =$ firstSink


main = defaultMain [
            bench "count elems from listSource in Identity" $ whnf (pure . countList) c,
            bench "count elems from listSource in IO" $ io $ countList $ c,
            bench "count elems in Trans from listSource in Identity" $ whnf (pure . countListTrans) c,
            bench "count elems in Trans from listSource in IO" $ io $ countListTrans $ c
    ] where c = 100000
