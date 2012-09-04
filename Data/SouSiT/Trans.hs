{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}

module Data.SouSiT.Trans (
    -- * Element Transformation
    map,
    mapWithState,
    zipWithIndex,
    --- * Take / Drop
    take,
    takeUntil,
    takeUntilEq,
    takeWhile,
    drop,
    dropUntil,
    dropWhile,
    -- * Filter / FlatMap
    filter,
    filterMap,
    flatMap,
    -- * Accumulation
    accumulate,
    buffer,
    -- * Dispersing
    disperse,
    -- * Chaining/Looping
    loop,
    loopN,
    sequence,
    andThen,
    -- * Handling of Either
    eitherRight,
    eitherLeft,
    -- * Utilities
    TransFun,
    applyTransFun,
    applyTransFun',
    mapSinkTransFun,
    mapSinkTransFun',
    toDoneTrans
) where

import Prelude hiding (map, take, takeWhile, drop, dropWhile, sequence, filter)
import qualified Prelude as P
import Data.SouSiT.Sink
import Data.SouSiT.Transform
import Control.Monad (liftM)


mapSinkStatus :: Monad m => (SinkStatus a m r -> SinkStatus b m r) -> Sink a m r -> Sink b m r
mapSinkStatus f = Sink . liftM f . sinkStatus

mapSinkTransFun f = mapSinkStatus (applyTransFun f)
mapSinkTransFun' ms mi = mapSinkStatus (applyTransFun' ms mi)

type TransFun a b m r = (a -> m (Sink a m r)) -> m r -> b -> m (Sink b m r)

applyTransFun :: Monad m => TransFun a b m r -> SinkStatus a m r -> SinkStatus b m r
applyTransFun _ (Done r) = Done r
applyTransFun f (Cont nf cf) = Cont (f nf cf) cf

applyTransFun' :: Monad m => (Sink a m r -> Sink b m r) -> (b -> a) -> SinkStatus a m r -> SinkStatus b m r
applyTransFun' ms mi = applyTransFun f
    where f nf _ = liftM ms . nf . mi

toDoneTrans :: Monad m => Sink a m r -> Sink a m r
toDoneTrans = mapSinkStatus fun
    where fun (Done r)   = Done r
          fun (Cont _ r) = Done r



-- | Transforms each input individually by applying the function.
map :: (a -> b) -> Transform a b
map f = mapSinkTransFun' (map f) f

-- | Transforms each input and carry a state between the inputs.
mapWithState :: (s -> a -> (b,s)) -> s -> Transform a b
mapWithState f s = mapSinkTransFun fun
    where fun nf _ i = let (i', s') = f s i in liftM (mapWithState f s') $ nf i'

-- | Transforms each input to a tuple (input, index of input).
-- I.e. for "Mario": (M, 0), (a, 1), (r, 2), (i, 3), (o, 4)
zipWithIndex :: Transform a (a, Int)
zipWithIndex = mapWithState fun 0
    where fun s i = ((i,s), s+1)


-- | Takes only the first n inputs, then returns done.
take :: (Num n, Ord n) => n -> Transform a a
take n | n > 0     = mapSinkTransFun' prev id
       | otherwise = toDoneTrans
    where prev = take (n - 1)

-- | Takes inputs until the input fullfils the predicate. The matching input is not passed on.
takeUntil :: (a -> Bool) -> Transform a a
takeUntil p = mapSinkStatus fun
    where fun (Done r) = Done r
          fun (Cont nf cf) = Cont nf' cf
            where nf' i | p i       = return $ doneSink cf
                        | otherwise = liftM (takeUntil p) (nf i)

-- | Takes inputs until the input matches the argument. The matching input is not passed on.
takeUntilEq :: Eq a => a -> Transform a a
takeUntilEq e = takeUntil (e ==)

-- | Take inputs while the input fullfils the predicate. As soon as the first non-matching input
-- is encountered no more inputs will be passed on.
takeWhile :: (a -> Bool) -> Transform a a
takeWhile f = takeUntil (not . f)

-- | Accumulates all elements with the accumulator function.
accumulate :: b -> (b -> a -> b) -> Transform a b
accumulate acc f = mapSinkStatus step
    where step (Done r)     = Done r
          step (Cont nf cf) = Cont nf' cf'
            where nf' i = return $ accumulate (f acc i) f $ contSink nf cf
                  cf' = nf acc >>= closeSink

-- | Accumulates up to n elements with the accumulator function and then releases it.
buffer :: Int -> b -> (b -> a -> b) -> Transform a b
buffer initN initAcc f | initN < 1 = error $ "Cannot buffer " ++ show initN ++ " elements"
                       | otherwise = step initN initAcc
    where step 1 acc = mapSinkStatus handle
                where handle (Done r)     = Done r
                      handle (Cont nf _) = Cont nf' cf'
                        where nf' i = liftM (step initN initAcc) $ nf (f acc i)
                              cf' = nf acc >>= closeSink
          step n acc = mapSinkStatus handle
                where handle (Done r)     = Done r
                      handle (Cont nf cf) = Cont nf' cf'
                        where nf' i = return $ step (n-1) (f acc i) $ contSink nf cf
                              cf' = nf acc >>= closeSink

-- | Yield all elements of the array as seperate outputs.
disperse :: Transform [a] a
disperse = mapSinkStatus f
    where f (Done r)     = Done r
          f (Cont nf cf) = Cont nf' cf
            where nf' is = liftM disperse $ feedList is $ contSink nf cf


-- | Drops the first n inputs then passes through all inputs unchanged
drop :: (Num n, Ord n) => n -> Transform a a
drop n0 = mapSinkStatus fun
    where fun (Done r) = Done r
          fun (Cont nf cf) = Cont (step n0) cf
            where step n i | n > 0     = return $ contSink (step $ n-1) cf
                           | otherwise = nf i

-- | Drops inputs until the predicate is matched. The matching input and all subsequent inputs
-- are passed on unchanged.
dropUntil :: (a -> Bool) -> Transform a a
dropUntil p = mapSinkTransFun fun
    where fun nf cf i | p i       = nf i
                      | otherwise = return $ dropUntil p $ contSink nf cf

-- | Drops inputs as long as they match the predicate. The first non-matching input and all
-- following inputs are passed on unchanged.
dropWhile :: (a -> Bool) -> Transform a a
dropWhile f = dropUntil (not . f)


-- | Only retains elements that match the filter function
filter :: (a -> Bool) -> Transform a a
filter p = mapSinkTransFun fun
    where fun nf cf i | p i       = liftM (filter p) $ nf i
                      | otherwise = return $ filter p $ contSink nf cf

-- | Map that allows to filter out elements.
filterMap :: (a -> Maybe b) -> Transform a b
filterMap f = mapSinkTransFun fun
    where fun nf cf i = case f i of
                            (Just i') -> liftM (filterMap f) $ nf i'
                            Nothing   -> return $ filterMap f $ contSink nf cf



-- | Executes with t1 and when t1 ends, then the next input is fed to through t2.
andThen :: Transform a b -> Transform a b -> Transform a b
andThen t1 t2 = (sinkUnwrap t2) . t1 . sinkWrap

data WrapRes i m r = SinkIsDone (m r)
                   | SinkIsCont (i -> m (Sink i m r)) (m r)

sinkUnwrap :: Monad m => Transform a b -> Sink a m (WrapRes b m r) -> Sink a m r
sinkUnwrap t = Sink . (>>= handle) . sinkStatus
    where handle (Cont nf cf) = return $ Cont (liftM (sinkUnwrap t) . nf) (cf >>= unwrapRes)
          handle (Done r)     = liftM (t . recSink) r >>= sinkStatus

sinkWrap :: Monad m => Sink i m r -> Sink i m (WrapRes i m r)
sinkWrap = Sink . liftM f . sinkStatus
    where f (Done r)     = Done $ return $ SinkIsDone r
          f (Cont nf cf) = Cont (liftM sinkWrap . nf) (return $ SinkIsCont nf cf)

recSink :: Monad m => WrapRes i m r -> Sink i m r
recSink (SinkIsDone r)     = doneSink r
recSink (SinkIsCont nf cf) = contSink nf cf

unwrapRes :: Monad m => WrapRes i m r -> m r
unwrapRes (SinkIsDone r)   = r
unwrapRes (SinkIsCont _ r) = r


-- | Executes the given transforms in a sequence, as soon as one ends the next input is
--   passed to the next transform.
sequence :: [Transform a b] -> Transform a b
sequence [] = error "No Transform for T.sequence"
sequence (t:[]) = t
sequence (t:ts) = andThen t (sequence ts)

-- | Loops the given transform forever.
loop :: Transform a b -> Transform a b
loop t = andThen t (loop t)

-- | Loops the given transform n times
loopN :: Int -> Transform a b -> Transform a b
loopN n t | n > 1  = andThen t $ loopN (n - 1) t
          | n == 1 = t
          | otherwise = error $ "Invalid n=" ++ show n ++ " in T.loopN"


-- | Applies a function to each element and passes on every element of the result list seperatly.
flatMap :: (a -> [b]) -> Transform a b
flatMap f = map f =$= disperse

-- | Only lets the 'rights' of Either pass.
eitherRight :: Transform (Either a b) b
eitherRight = mapSinkStatus f
    where f (Done r)     = Done r
          f (Cont nf cf) = Cont (liftM eitherRight . handle) cf
            where handle = either (return . ignore) nf
                  ignore _ = contSink nf cf

-- | Only lets the 'lefts' of Either pass.
eitherLeft :: Transform (Either a b) a
eitherLeft = mapSinkStatus f
    where f (Done r)     = Done r
          f (Cont nf cf) = Cont (liftM eitherLeft . handle) cf
            where handle = either nf (return . ignore)
                  ignore _ = contSink nf cf
