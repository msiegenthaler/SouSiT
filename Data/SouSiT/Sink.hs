{-# LANGUAGE Rank2Types, BangPatterns #-}
module Data.SouSiT.Sink (
    Sink(..),
    SinkStatus(..),
    closeSink,
    -- * monadic functions
    input,
    skip,
    -- * utility functions
    appendSink,
    (=||=),
    feedList,
    -- * sink construction
    contSink,
    doneSink,
    doneSink',
    actionSink,
    openCloseActionSink,
    maybeSink,
) where

import Data.Monoid
import Control.Applicative
import Control.Monad


--- | Sink for data. Aggregates data to produce a single (monadic) result.
data Sink i m r = Sink { sinkStatus :: m (SinkStatus i m r) }

data SinkStatus i m r = Cont (i -> Sink i m r) (m r)
                      | Done (m r)

instance Monad m => Functor (Sink i m) where
    fmap f (Sink st) = Sink (liftM mp st)
        where mp (Done r)  = Done (liftM f r)
              mp (Cont nf cf) = Cont (fmap f . nf) (liftM f cf)

instance Monad m => Monad (Sink i m) where
    return a = doneSink $ return a
    (Sink st) >>= f = Sink (st >>= mp)
        where mp (Done r) = liftM f r >>= sinkStatus
              mp (Cont nf _) = return $ Cont ((>>= f) . nf) noResult

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


-- | Reads a value.
input :: Monad m => Sink a m a
input = Sink (return $ Cont f noResult)
    where f = Sink . return . Done . return

-- | Skips n input values.
skip :: (Eq n, Num n, Monad m) => n -> Sink a m ()
skip 0 = return ()
skip i = input >> skip (i-1)



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
feedList :: Monad m => [i] -> Sink i m r -> Sink i m r
feedList [] !s = s
feedList (x:xs) !s = Sink (sinkStatus s >>= step)
    where step (Done r)   = return $ Done r
          step (Cont f _) = sinkStatus $ feedList xs $ f x

contSink :: Monad m => (i -> Sink i m r) -> m r -> Sink i m r
contSink next = Sink . return . Cont next

doneSink :: Monad m => m r -> Sink i m r
doneSink = Sink . return . Done

doneSink' :: Monad m => r -> Sink i m r
doneSink' = Sink . return . Done . return


-- | Sink that executes a monadic action per input received. Does not terminate.
actionSink :: Monad m => (i -> m ()) -> Sink i m ()
actionSink process = contSink f (return ())
    where f i = Sink $ process i >> sinkStatus (actionSink process)

-- | First calls open, then processes every input with process and when the sink is closed
--   close is called. Does not terminate.
openCloseActionSink :: Monad m => m a -> (a -> m ()) -> (a -> i -> m ()) -> Sink i m ()
openCloseActionSink open close process = contSink first (return ())
    where first i = Sink $ open >>= flip step i
          step rs i = process rs i >> return (Cont (Sink . step rs) (close rs))

-- | Sink that executes f for every input.
--   The sink continues as long as the action returns Nothing, when the action returns
--   Just, then that value is the result of the sink (and the sink is 'full').
maybeSink :: Monad m => (i -> m (Maybe r)) -> Sink i m (Maybe r)
maybeSink f = contSink step (return Nothing)
    where step i = Sink $ liftM cont (f i)
          cont Nothing = Cont step (return Nothing)
          cont result  = Done $ return result
