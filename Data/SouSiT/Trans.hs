{-# LANGUAGE GADTs #-}

module Data.SouSiT.Trans (
    -- * Element Transformation
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
    -- * Filter
    filter,
    -- * Accumulation
    accumulate,
    buffer,
    -- * Dispersing
    disperse,
    -- * Chaining/Looping
    loop,
    loopN,
    sequence,
    -- * Handling of Either
    eitherRight,
    eitherLeft
) where

import Prelude hiding (id, map, take, takeWhile, drop, dropWhile, sequence, filter)
import qualified Prelude as P
import Control.Monad hiding (sequence)
import Data.SouSiT


-- | Does not perform any transformation.
id :: Transform a a
id = IdentTransform

-- | Transforms each input individually by applying the function.
map :: (a -> b) -> Transform a b
map = MappingTransform

-- | Transforms each input to a tuple (input, index of input).
-- I.e. for "Mario": (M, 0), (a, 1), (r, 2), (i, 3), (o, 4)
zipWithIndex :: Transform a (a, Int)
zipWithIndex = ContTransform (step 0) []
    where step nr i =([(i, nr)], ContTransform (step (succ nr)) [])

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

-- | Yield all elements of the array as seperate outputs.
disperse :: Transform [a] a
disperse = ContTransform (\i -> (i, disperse)) []

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


-- | Only retains elements that match the filter function
filter :: (a -> Bool) -> Transform a a
filter f = ContTransform step []
    where step i | f i       = ([i], filter f)
                 | otherwise = ([],  filter f)


-- | Loops the given transform forever.
loop :: Transform a b -> Transform a b
loop = sequence . repeat

-- | Loops the given transform n times
loopN :: Int -> Transform a b -> Transform a b
loopN n =  sequence . P.take n . repeat

-- | Executes the given transforms in a sequence, as soon as one is EndTransform the next input
-- is passed to the next transform.
sequence :: [Transform a b] -> Transform a b
sequence = sequence2 []

sequence2 :: [b] -> [Transform a b] -> Transform a b
sequence2 r  [] = EndTransform r
sequence2 [] (IdentTransform:_) = IdentTransform
sequence2 r  (IdentTransform:_) = ContTransform (\i -> (r ++ [i], IdentTransform)) r
sequence2 [] ((MappingTransform f):_) = MappingTransform f
sequence2 r  ((MappingTransform f):_) = ContTransform (\i -> (r ++ [f i], MappingTransform f)) r
sequence2 r  ((EndTransform r2):ts) = sequence2 (r ++ r2) ts
sequence2 r  ((ContTransform next done):ts) = ContTransform step (r ++ done)
    where step i = let (es,t') = next i in (r ++ es, sequence2 [] (t':ts))

-- | Only lets the 'rights' of Either pass.
eitherRight :: Transform (Either a b) b
eitherRight = ContTransform step []
    where step (Right a) = ([a], eitherRight)
          step _         = ([],  eitherRight)

-- | Only lets the 'lefts' of Either pass.
eitherLeft :: Transform (Either a b) a
eitherLeft = ContTransform step []
    where step (Left a) = ([a], eitherLeft)
          step _        = ([],  eitherLeft)
