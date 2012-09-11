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

pure = runIdentity
io :: IO a -> IO a
io = id


countList :: Monad m => Int -> m Int
countList n = elemCountSource n $$ countSink

countListTrans :: Monad m => Int -> m Int
countListTrans n = elemCountSource n $$ T.count =$ firstSink

dropListFew :: Monad m => Int -> m Int
dropListFew n = elemCountSource n $$ T.drop 100 =$ countSink

dropListMany :: Monad m => Int -> m Int
dropListMany n = elemCountSource n $$ T.drop (n-100) =$ countSink

dropUntilListFew :: Monad m => Int -> m Int
dropUntilListFew n = elemCountSource n $$ T.dropUntil (>100) =$ countSink

dropUntilListMany :: Monad m => Int -> m Int
dropUntilListMany n = elemCountSource n $$ T.dropUntil (>(n-100)) =$ countSink

takeListFew :: Monad m => Int -> m Int
takeListFew n = elemCountSource n $$ T.take 100 =$ countSink

takeListMany :: Monad m => Int -> m Int
takeListMany n = elemCountSource n $$ T.take (n-100) =$ countSink

takeUntilListFew :: Monad m => Int -> m Int
takeUntilListFew n = elemCountSource n $$ T.takeUntil (>100) =$ countSink

takeUntilListMany :: Monad m => Int -> m Int
takeUntilListMany n = elemCountSource n $$ T.takeUntil (>(n-100)) =$ countSink


main = defaultMain [
            bgroup "Monad comparision" [
                bench "count elems from listSource in Identity" $ whnf (pure . countList) c,
                bench "count elems from listSource in IO" $ io $ countList c,
                bench "count elems in Trans from listSource in Identity" $ whnf (pure . countListTrans) c,
                bench "count elems in Trans from listSource in IO" $ io $ countListTrans c
            ],
            bgroup "Transforms" [
                bench "drop few elems" $ io $ dropListFew c,
                bench "drop many elems" $ io $ dropListMany c,
                bench "dropUntil few elems" $ io $ dropUntilListFew c,
                bench "dropUntil many elems" $ io $ dropUntilListMany c,
                bench "take few elems" $ io $ takeListFew c,
                bench "take many elems" $ io $ takeListMany c,
                bench "takeUntil few elems" $ io $ takeUntilListFew c,
                bench "takeUntil many elems" $ io $ takeUntilListMany c
            ]
    ] where c = 100000
