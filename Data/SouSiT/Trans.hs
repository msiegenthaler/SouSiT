{-# LANGUAGE Rank2Types, ImpredicativeTypes, BangPatterns #-}

module Data.SouSiT.Trans (
    -- * Element Transformation
    map,
    mapM,
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
    count,
    -- * Dispersing
    disperse,
    -- * Chaining/Looping
    andThen,
    loop,
    loopN,
    sequence,
    -- * Handling of Either
    eitherRight,
    eitherLeft,
    -- * Serialization (cereal)
    serialize,
    deserialize,
    -- * Utilities
    mapSinkStatus,
    TransFun,
    applyTransFun,
    mapSinkTransFun,
    applyMapping,
    mapSinkMapping,
    toDoneTrans,
    debug
) where

import Prelude hiding (map, mapM, take, takeWhile, drop, dropWhile, sequence, filter)
import qualified Prelude as P
import Data.SouSiT.Sink
import Data.SouSiT.Transform
import Data.ByteString (ByteString)
import qualified Data.Serialize as S
import Control.Monad (liftM)
import Control.Monad.IO.Class


mapSinkStatus :: Monad m => (SinkStatus a m r -> SinkStatus b m r) -> Sink a m r -> Sink b m r
mapSinkStatus f = Sink . liftM f . sinkStatus


type TransFun a b m r = (a -> Sink a m r) -> m r -> b -> Sink b m r

applyTransFun :: Monad m => TransFun a b m r -> SinkStatus a m r -> SinkStatus b m r
applyTransFun _ (Done r) = Done r
applyTransFun f (Cont nf cf) = Cont (f nf cf) cf

mapSinkTransFun f = mapSinkStatus (applyTransFun f)


applyMapping :: Monad m => (Sink a m r -> Sink b m r) -> (b -> a) -> SinkStatus a m r -> SinkStatus b m r
applyMapping _ _ (Done r) = Done r
applyMapping ms mi (Cont nf cf) = Cont (ms . nf . mi) cf

mapSinkMapping ms mi = mapSinkStatus $ applyMapping ms mi


toDoneTrans :: Monad m => Sink a m r -> Sink a m r
toDoneTrans = mapSinkStatus fun
    where fun (Done r)   = Done r
          fun (Cont _ r) = Done r


-- | Transforms each input individually by applying the function.
map :: (a -> b) -> Transform a b
map f = mapSinkMapping (map f) f

-- | Transforms each input individually by applying the monadic function.
--   Warning: This is not really a Transform, since it isn't pure.
mapM :: Monad m => (b -> m a) -> Sink a m r -> Sink b m r
mapM action sink = Sink (liftM f (sinkStatus sink))
    where f (Done r) = Done r
          f (Cont nf cf) = Cont nf' cf
              where nf' i = mapM action $ Sink $ action i >>= sinkStatus . nf

-- | Transforms each input and carry a state between the inputs.
mapWithState :: (s -> a -> (b,s)) -> s -> Transform a b
mapWithState f !s = mapSinkTransFun fun
    where fun nf _ i = let (i', s') = f s i in mapWithState f s' (nf i')

-- | Transforms each input to a tuple (input, index of input).
-- I.e. for "Mario": (M, 0), (a, 1), (r, 2), (i, 3), (o, 4)
zipWithIndex :: Transform a (a, Int)
zipWithIndex = mapWithState fun 0
    where fun s i = ((i,s), s+1)

-- | Takes only the first n inputs, then returns done.
take :: (Num n, Ord n) => n -> Transform a a
take n | n > 0     = mapSinkMapping prev id
       | otherwise = toDoneTrans
    where prev = take (n - 1)

-- | Takes inputs until the input fullfils the predicate. The matching input is not passed on.
takeUntil :: (a -> Bool) -> Transform a a
takeUntil p = mapSinkStatus fun
    where fun s@(Done _) = s
          fun (Cont nf cf) = Cont nf' cf
            where nf' i = if p i then doneSink cf
                          else takeUntil p (nf i)

-- | Takes inputs until the input matches the argument. The matching input is not passed on.
takeUntilEq :: Eq a => a -> Transform a a
takeUntilEq e = takeUntil (e ==)

-- | Take inputs while the input fullfils the predicate. As soon as the first non-matching input
-- is encountered no more inputs will be passed on.
takeWhile :: (a -> Bool) -> Transform a a
takeWhile f = takeUntil (not . f)


-- | Accumulates all elements with the accumulator function.
accumulate :: b -> (b -> a -> b) -> Transform a b
accumulate initAcc f = mapSinkStatus fun
    where fun (Done r) = Done r
          fun (Cont nf _) = step initAcc
            where step !acc = Cont (Sink . return . step . f acc) (closeSink (nf acc))

-- | Counts the received elements.
count :: Num n => Transform a n
count = accumulate 0 step
    where step i _ = i + 1

-- | Accumulates up to n elements with the accumulator function and then releases it.
buffer :: Int -> b -> (b -> a -> b) -> Transform a b
buffer initN initAcc f = if initN > 0 then mapSinkStatus fun
                                      else error $ "Cannot buffer " ++ show initN ++ " elements"
    where fun (Done r) = Done r
          fun (Cont nf _) = step initN initAcc
            where step 1 !acc = Cont nf' (closeSink (nf acc))
                        where nf' i = Sink $ liftM fun $ sinkStatus $ nf (f acc i)
                  step n !acc = Cont (Sink . return . step (n-1) . f acc) (closeSink (nf acc))

-- | Yield all elements of the array as seperate outputs.
disperse :: Transform [a] a
disperse sink = Sink $ liftM fun (sinkStatus sink)
    where fun (Done r) = Done r
          fun (Cont _ cf) = Cont (disperse . flip feedList sink) cf


-- | Drops the first n inputs then passes through all inputs unchanged
drop :: (Num n, Ord n) => n -> Transform a a
drop n0 = mapSinkStatus fun
    where fun (Done r) = Done r
          fun (Cont nf cf) = Cont (step n0) cf
            where step n i | n > 0     = contSink (step $ n-1) cf
                           | otherwise = nf i

-- | Drops inputs until the predicate is matched. The matching input and all subsequent inputs
-- are passed on unchanged.
dropUntil :: (a -> Bool) -> Transform a a
dropUntil p = mapSinkTransFun fun
    where fun nf cf i | p i       = nf i
                      | otherwise = dropUntil p $ contSink nf cf

-- | Drops inputs as long as they match the predicate. The first non-matching input and all
-- following inputs are passed on unchanged.
dropWhile :: (a -> Bool) -> Transform a a
dropWhile f = dropUntil (not . f)


-- | Only retains elements that match the filter function
filter :: (a -> Bool) -> Transform a a
filter p = mapSinkTransFun fun
    where fun nf cf i | p i       = filter p $ nf i
                      | otherwise = filter p $ contSink nf cf

-- | Map that allows to filter out elements.
filterMap :: (a -> Maybe b) -> Transform a b
filterMap f = mapSinkTransFun fun
    where fun nf cf i = case f i of
                            (Just i') -> filterMap f $ nf i'
                            Nothing   -> filterMap f $ contSink nf cf

-- | Executes with t1 and when t1 ends, then the next input is fed to through t2.
andThen :: Transform a b -> Transform a b -> Transform a b
andThen t1 t2 sink = Sink $ (>>= f) $ sinkStatus sink
    where f (Done r)   = return $ Done r
          f (Cont _ _) = sinkStatus $ sinkUnwrap t2 $ t1 $ sinkWrap sink

data WrapRes i m r = SinkIsDone (m r)
                   | SinkIsCont (i -> Sink i m r) (m r)

sinkUnwrap :: Monad m => Transform a b -> Sink a m (WrapRes b m r) -> Sink a m r
sinkUnwrap t = Sink . (>>= handle) . sinkStatus
    where handle (Cont nf cf) = return $ Cont (sinkUnwrap t . nf) (cf >>= unwrapRes)
          handle (Done r)     = liftM (t . recSink) r >>= sinkStatus

sinkWrap :: Monad m => Sink i m r -> Sink i m (WrapRes i m r)
sinkWrap = Sink . liftM f . sinkStatus
    where f (Done r)     = Done $ return $ SinkIsDone r
          f (Cont nf cf) = Cont (sinkWrap . nf) (return $ SinkIsCont nf cf)

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
          f (Cont nf cf) = Cont (eitherRight . handle) cf
            where handle = either ignore nf
                  ignore _ = contSink nf cf

-- | Only lets the 'lefts' of Either pass.
eitherLeft :: Transform (Either a b) a
eitherLeft = mapSinkStatus f
    where f (Done r)     = Done r
          f (Cont nf cf) = Cont (eitherLeft . handle) cf
            where handle = either nf ignore
                  ignore _ = contSink nf cf


-- | Outputs every element received and the result to the System-out (using putStrLn).
--   Format: <label>: <element>
--           <label> is <result>
debug :: (Show a, Show r, MonadIO m) => String -> Sink a m r -> Sink a m r
debug label sink = mapM f sink >>= g
    where f i = output (label ++ ": " ++ show i) >> return i
          g r = doneSink $ output (label ++ " is " ++ show r) >> return r
          output = liftIO . putStrLn


-- | Serialize the elements into ByteString using cereal. For every input there is exactly one
--   output.
serialize :: S.Serialize a => Transform a ByteString
serialize = map S.encode

-- | Deserializes ByteString elements. The ByteStrings may be chunked, but the beginnings
--   of values must be aligned to the chunks. If this is not the case then consider splitting
--   the ByteStrings by the appropriate start delimiter (if available) or split them up into
--   singletons.
deserialize :: S.Serialize b => Transform ByteString b
deserialize = deserialize' []

deserialize' :: S.Serialize b => [ByteString -> S.Result b] -> Transform ByteString b
deserialize' ips = mapSinkTransFun fun
    where fun nf cf i = deserialize' ps' $ feedList bs $ contSink nf cf
            where (ps', bs) = tryToParse (S.runGetPartial S.get:ips) i
          tryToParse [] _ = ([],[])
          tryToParse (p:ps) i = case p i of
                    (S.Done b _)   -> (ps',  b:bs)
                    (S.Fail _)     -> (ps',  bs)
                    (S.Partial p') -> (p':ps,bs)
            where (ps', bs) = tryToParse ps i
