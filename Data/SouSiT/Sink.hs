{-# LANGUAGE Rank2Types, BangPatterns #-}
module Data.SouSiT.Sink (
    Sink(..),
    SinkStatus(..),
    closeSink,
    -- * monadic functions
    input,
    inputOr,
    inputMap,
    inputMaybe,
    skip,
    -- * utility functions
    appendSink,
    (=||=),
    feedList,
    liftSink,
    -- * sink construction
    contSink,
    contSink',
    doneSink,
    doneSink',
    actionSink,
    openCloseActionSink,
    maybeSink
) where

import Data.Monoid
import Control.Applicative
import Control.Monad


--- | Sink for data. Aggregates data to produce a single (monadic) result.
data Sink i m r = Sink { sinkStatus :: m (SinkStatus i m r) }

data SinkStatus i m r = Cont (i -> m (Sink i m r)) (m r)
                      | Done (m r)

instance Monad m => Functor (Sink i m) where
    fmap f (Sink st) = Sink (liftM mp st)
        where mp (Done r)  = Done (liftM f r)
              mp (Cont nf cf) = Cont (liftM (fmap f) . nf) (liftM f cf)

instance Monad m => Monad (Sink i m) where
    return a = doneSink $ return a
    (Sink st) >>= f = Sink (st >>= mp)
        where mp (Done r) = liftM f r >>= sinkStatus
              mp (Cont nf cf) = return $ Cont (liftM (>>= f) . nf) (cf >>= closeSink . f)

instance Monad m => Applicative (Sink i m) where
    pure = return
    af <*> s = do f <- af
                  v <- s
                  return (f v)

noResult :: Monad m => m a
noResult = fail "no result: not enough input"


-- | Closes the sink and returns its result.
closeSink :: Monad m => Sink i m r -> m r
closeSink (Sink st) = st >>= handle
    where handle (Done r) = r
          handle (Cont _ r) = r


-- | Reads the next element.
--   If the sink is closed while waiting for the input, then the parameter is returned
--   as the sinks result.
inputOr :: Monad m => m a -> Sink a m a
inputOr = contSink' doneSink'

-- | Reads the next element. Returns (Just a) for the element or Nothing if the sink is closed
--   before the input was available.
inputMaybe :: Monad m => Sink a m (Maybe a)
inputMaybe = inputMap (return . Just) (return Nothing)

-- | Reads the next element. Returns (Just a) for the element or Nothing if the sink is closed
--   before the input was available.
inputMap :: Monad m => (a -> m b) -> m b -> Sink a m b
inputMap f = contSink' (doneSink . f)

-- | Reads the next element.
--   The sink returns a fail if it is closed before the input is received.
input :: Monad m => Sink a m a
input = inputOr noResult

-- | Skips n input elements. If the sink is closed before then the result will also be ().
skip :: (Eq n, Num n, Monad m) => n -> Sink a m ()
skip 0 = doneSink (return ())
skip n = contSink' f (return ())
    where f _ = skip (n-1)


-- | Concatenates two sinks that produce a monoid.
(=||=) :: (Monad m, Monoid r) => Sink a m r -> Sink a m r -> Sink a m r
(=||=) = appendSink
infixl 3 =||=

-- | Concatenates two sinks that produce a monoid.
appendSink :: (Monad m, Monoid r) => Sink a m r -> Sink a m r -> Sink a m r
appendSink s1 s2 = do r1 <- s1
                      r2 <- s2
                      return $ mappend r1 r2

-- | Feed a list of inputs to a sink.
feedList :: Monad m => [i] -> Sink i m r -> m (Sink i m r)
feedList [] !s = return s
feedList (x:xs) !s = sinkStatus s >>= step
    where step (Done _) = return s
          step (Cont nf _) = nf x >>= feedList xs


contSink :: Monad m => (i -> m (Sink i m r)) -> m r -> Sink i m r
contSink next = Sink . return . Cont next

contSink' :: Monad m => (i -> Sink i m r) -> m r -> Sink i m r
contSink' next = contSink (return . next)

doneSink :: Monad m => m r -> Sink i m r
doneSink = Sink . return . Done

doneSink' :: Monad m => r -> Sink i m r
doneSink' = Sink . return . Done . return


-- | Sink that executes a monadic action per input received. Does not terminate.
actionSink :: Monad m => (i -> m ()) -> Sink i m ()
actionSink process = contSink step (return ())
    where step i = process i >> return (actionSink process)

-- | First calls open, then processes every input with process and when the sink is closed
--   close is called. Does not terminate.
openCloseActionSink :: Monad m => m a -> (a -> m ()) -> (a -> i -> m ()) -> Sink i m ()
openCloseActionSink open close process = contSink first (return ())
    where first i = open >>= flip step i
          step rs i = process rs i >> return (contSink (step rs) (close rs))

-- | Sink that executes f for every input.
--   The sink continues as long as the action returns Nothing, when the action returns
--   Just, then that value is the result of the sink (and the sink is 'full').
maybeSink :: Monad m => (i -> m (Maybe r)) -> Sink i m (Maybe r)
maybeSink process = contSink step (return Nothing)
    where step i = process i >>= cont
          cont Nothing = return $ maybeSink process
          cont result  = return $ doneSink' result


-- | Changes the monad of a sink based upon a conversion function that maps the original monad
--   to the new one.
liftSink :: (Monad m, Monad m') => (forall x . m x -> m' x) -> Sink i m r -> Sink i m' r
liftSink t sink = Sink $ t (sinkStatus sink >>= trans)
    where trans (Done r) = return $ Done $ t r
          trans (Cont nf cf) = return $ Cont nf' (t cf)
            where nf' i = liftM (liftSink t) (t $ nf i)
