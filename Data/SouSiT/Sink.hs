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
    decorateSink,
    actionSink,
    openCloseActionSink,
    maybeSink,
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
              mp (Cont nf _) = return $ Cont (liftM (>>= f) . nf) noResult

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
    where f i = let r = return i in 
            return $ Sink (return $ Done r)

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
feedList :: Monad m => [i] -> Sink i m r -> m (Sink i m r)
feedList [] s = return s
feedList (x:xs) s = sinkStatus s >>= step
    where step (Done _) = return s
          step (Cont f _) = f x >>= feedList xs

contSink :: Monad m => (i -> m (Sink i m r)) -> m r -> Sink i m r
contSink next close = Sink (return $ Cont next close)

doneSink :: Monad m => m r -> Sink i m r
doneSink result = Sink (return $ Done result)

doneSink' :: Monad m => r -> Sink i m r
doneSink' = doneSink . return

-- | Decorates a Sink with a monadic function. Can be used to produce debug output and such.
decorateSink :: Monad m => (i -> m ()) -> Sink i m r -> Sink i m r
decorateSink df = Sink . liftM step . sinkStatus
    where step (Done r) = Done r
          step (Cont nf cf) = Cont nf' cf
            where nf' i = liftM (decorateSink df) (df i >> nf i)

-- | Sink that executes a monadic action per input received. Does not terminate.
actionSink :: Monad m => (i -> m ()) -> Sink i m ()
actionSink process = contSink f (return ())
    where f i = process i >> return (actionSink process)

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
maybeSink f = contSink step (return Nothing)
  where step i = f i >>= cont
        cont Nothing = return $ maybeSink f
        cont result = return $ doneSink' result
