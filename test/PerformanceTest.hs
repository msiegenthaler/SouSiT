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
  where step n = contSink f (return n)
          where f _ = return $ step (n + 1)

firstSink :: Monad m => Sink a m a
firstSink = input

elemCountSource n = listSource [1..n]


countList n = runIdentity (elemCountSource n $$ countSink) 

countListIO :: Int -> IO Int
countListIO n = elemCountSource n $$ countSink

countListTrans n = runIdentity (elemCountSource n $$ T.count =$ firstSink)

countListTransIO :: Int -> IO Int
countListTransIO n = elemCountSource n $$ T.count =$ firstSink


main = defaultMain [
            bench "count elems from listSource in Identity" $ whnf countList c,
            bench "count elems from listSource in IO" $ countListIO c,
            bench "count elems in Trans from listSource in Identity" $ whnf countListTrans c,
            bench "count elems in Trans from listSource in IO" $ countListTransIO c
    ] where c = 100000
