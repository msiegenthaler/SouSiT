{-# LANGUAGE GADTs #-}

module Data.SouSiT.Trans (
    -- * Element Tranformers
    id,
    map,
    zipWithIndex,
    --- * Take / Drop
    take,
    takeUntil,
    takeUntilEq,
    takeWhile,
    drop,
    dropUntil,
    dropWhile,
    -- * Accumulation
    accumulate,
    buffer,
    -- * Looping
    loop,
    loopN
) where

import Prelude hiding (id, map, take, takeWhile, drop, dropWhile)
import qualified Prelude as P
import Control.Monad
import Data.SouSiT


-- | Does not perform any transformation.
id :: Transform a a
id = IdentTransform

-- | Transforms each input individually by applying the function.
map :: (a -> b) -> Transform a b
map = MappingFunTransform

-- | Transforms each input to a tuple (input, index of input).
-- I.e. for "Mario": (M, 0), (a, 1), (r, 2), (i, 3), (o, 4)
zipWithIndex :: Transform a (a, Int)
zipWithIndex = MappingTransform $ step 0
    where step nr i = ((i, nr), MappingTransform $ step (succ nr))

-- | Takes only the first n inputs, then returns done.
take :: (Num n, Ord n) => n -> Transform a a
take n | n > 0     = ContTransform (\i -> ([i], take $ n - 1)) []
       | otherwise = EndTransform []

-- | Takes inputs until the input fullfils the predicate. The matching input is not passed on.
takeUntil :: (a -> Bool) -> Transform a a
takeUntil p = ContTransform (step []) []
    where step sf e | p e       = (sf, EndTransform [])
                    | otherwise = ([], ContTransform (step sf') sf')
                where sf' = sf ++ [e]

-- | Takes inputs until the input matches the argument. The matching input is not passed on.
takeUntilEq :: Eq a => a -> Transform a a
takeUntilEq e = takeUntil (e ==)

-- | Take inputs while the input fullfils the predicate. As soon as the first non-matching input
-- is encountered no more inputs will be passed on.
takeWhile :: (a -> Bool) -> Transform a a
takeWhile f = takeUntil (not . f)

-- | Accumulates all elements with the accumulator function.
accumulate :: b -> (b -> a -> b) -> Transform a b
accumulate acc f = ContTransform step [acc]
    where step i = ([], accumulate (f acc i) f)

-- | Accumulates up to n elements with the accumulator function and then releases it.
buffer :: Int -> b -> (b -> a -> b) -> Transform a b
buffer initN initAcc f | initN < 1 = error $ "Cannot buffer " ++ show initN ++ " elements"
                       | otherwise = step initN initAcc
    where step 1 acc = ContTransform next [acc]
            where next i = ([f acc i], step initN initAcc)
          step n acc = ContTransform next [acc] 
            where next i = ([], step (n-1) (f acc i))

-- | Drops the first n inputs then passes through all inputs unchanged
drop :: (Num n, Ord n) => n -> Transform a a
drop n | n > 0     = ContTransform (\_ -> ([], drop (n - 1))) []
       | otherwise = IdentTransform

-- | Drops inputs until the predicate is matched. The matching input and all subsequent inputs
-- are passed on unchanged.
dropUntil :: (a -> Bool) -> Transform a a
dropUntil p = ContTransform step []
    where step i | p i       = ([i], IdentTransform)
                 | otherwise = ([],  ContTransform step [])

-- | Drops inputs as long as they match the predicate. The first non-matching input and all
-- following inputs are passed on unchanged.
dropWhile :: (a -> Bool) -> Transform a a
dropWhile f = dropUntil (not . f)

-- | Loops the given transform forever.
loop :: Transform a b -> Transform a b
loop (EndTransform r) = ContTransform step r
    where step _ = (r, ContTransform step r)
loop (ContTransform on od) = ContTransform (conv on) od
    where conv next i = let (es, nt) = next i in case nt of
                    IdentTransform            -> (es, IdentTransform)
                    t@(MappingFunTransform _) -> (es, t)
                    t@(MappingTransform _)    -> (es, t)
                    (ContTransform n d)       -> (es, ContTransform (conv n) d)
                    (EndTransform r)          -> (es ++ r, ContTransform (conv on) od)
loop t = t

-- | Loops the given transform n times
loopN :: (Num n, Ord n) => n -> Transform a b -> Transform a b
loopN n (EndTransform r) = ContTransform (step n) r
    where step n _ | n > 0     = (r, ContTransform (step (n-1)) r)
                   | otherwise = (r, (EndTransform []))
loopN n (ContTransform on od) = ContTransform (conv n on) od
    where conv n next i = let (es, nt) = next i in case nt of
                    IdentTransform            -> (es, IdentTransform)
                    t@(MappingFunTransform _) -> (es, t)
                    t@(MappingTransform _)    -> (es, t)
                    (ContTransform nf d)      -> (es, ContTransform (conv n nf) d)
                    (EndTransform r)          ->
                        if n > 1 then (es ++ r, ContTransform (conv (n-1) on) od)
                        else (es ++ r, EndTransform [])
loopN _ t = t
