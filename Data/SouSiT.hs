module Data.SouSiT (
    -- * Sink
    Sink,
    Fetch,
    input,
    skip,
    liftSink,
    liftFetch,
    -- * Source
    Source,
    transfer,
    FeedSource,
    SimpleSource(..),
    feedToSink,
    ($$),
    (=+=),
    (=+|=),
    -- * Transform
    Transform,
    (=$=),
    (=$),
    ($=)
) where

import Data.SouSiT.Sink
import Data.SouSiT.Source
import Data.SouSiT.Transform
import Control.Monad.Identity


type Fetch i a = Sink i Identity a

-- | Lift the (pure) fetch sink into any monad.
liftFetch :: Monad m => Fetch i a -> Sink i m a
liftFetch = liftSink (return . runIdentity)
