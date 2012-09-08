module Data.SouSiT (
    -- * Sink
    Sink,
    liftSink,
    Fetch,
    liftFetch,
    input,
    skip,
    -- * Source
    Source,
    Source2,
    transfer,
    BasicSource(..),
    BasicSource2(..),
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

liftFetch :: Monad m => Fetch i a -> Sink i m a
liftFetch fetch = liftSink (return . runIdentity) fetch
