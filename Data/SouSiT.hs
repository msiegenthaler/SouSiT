module Data.SouSiT (
    -- * Sink
    Sink,
    Fetch,
    input,
    skip,
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
