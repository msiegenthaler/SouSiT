{-# LANGUAGE MultiParamTypeClasses #-}

module Data.SouSiT.Trans (
    -- * Element Tranformers
    id,
    map,
    zipWithIndex,
    --- * Take / Drop
    take,
    takeUntil,
    takeUntilEq,
    drop,
    dropUntil,
    -- * Accumulation
    accumulate,
    buffer
) where

import Prelude hiding (take, map, id, drop)
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
