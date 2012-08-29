{-# LANGUAGE GADTs #-}

module Data.SouSiT.Transform (
    Transform(..),
    transformSink,
    transformSource,
    mergeTransform,
    (=$=),
    (=$),
    ($=)
) where

import Data.SouSiT.Sink
import Data.SouSiT.Source
import Control.Monad
import qualified Control.Category as C


-- | A transformation onto a sink
data Transform a b where
    IdentTransform      :: Transform a a
    MappingTransform :: (a -> b) -> Transform a b
    ContTransform       :: (a -> ([b], Transform a b)) -> [b] -> Transform a b
    EndTransform        :: [b] -> Transform a b

instance C.Category Transform where
    id  = MappingTransform id
    (.) = flip mergeTransform

-- | apply a transform to a sink
(=$) :: (Monad m) => Transform a b -> Sink b m r -> Sink a m r
(=$)  = transformSink
infixl 2 =$

-- | apply a transform to a source
($=) :: (Source src, Monad m) => src m a -> Transform a b -> BasicSource m b
($=) = flip transformSource
infixl 1 $=

-- | merges two transforms into one
(=$=) :: Transform a b -> Transform b c -> Transform a c
(=$=) = mergeTransform

-- | apply transform to source
transformSource :: (Source src, Monad m) => Transform a b -> src m a -> BasicSource m b
transformSource IdentTransform src = BasicSource $ transfer src
transformSource t src = BasicSource $ transfer src . transformSink t

-- | Apply a transform to a sink
transformSink :: Monad m => Transform a b -> Sink b m r -> Sink a m r
transformSink IdentTransform s = s
transformSink (MappingTransform f) s = transformSinkMapping f s
transformSink (ContTransform tfn tfe) s = Sink $ liftM step $ sinkStatus s
    where step (Done r) = Done r
          step _ = Cont nf' cf'
            where nf' i = liftM (transformSink trans') (feedList es s)
                    where (es, trans') = tfn i
                  cf' = feedList tfe s >>= closeSink
transformSink (EndTransform es) s = Sink $ liftM step $ sinkStatus s
    where step (Done r) = Done r
          step _ = Done $ feedList es s >>= closeSink

transformSinkMapping :: Monad m => (a -> b) -> Sink b m r -> Sink a m r
transformSinkMapping f = Sink . liftM step . sinkStatus
    where step (Done r) = Done r
          step (Cont nf cf) = Cont nf' cf
            where nf' i = liftM (transformSinkMapping f) $ nf (f i)

-- | merges two transforms into one
mergeTransform :: Transform a b -> Transform b c -> Transform a c
mergeTransform IdentTransform t = t
mergeTransform t IdentTransform = t
mergeTransform (MappingTransform f1) (MappingTransform f2) = MappingTransform (f2 . f1)
mergeTransform _ (EndTransform r) = EndTransform r
mergeTransform (EndTransform r) t2 = EndTransform $ endTransform $ feedTransform r t2
mergeTransform (ContTransform f d) t2 = ContTransform next' done'
    where next' i = (bs, mergeTransform t1' t2')
            where (as, t1') = f i
                  (bs, t2') = feedTransform as t2
          done' = endTransform $ feedTransform d t2
mergeTransform t1 t2 = mergeTransform (contEndOnly t1) (contEndOnly t2)

-- | Converts all transformers to either ContTransform or EndTransform
contEndOnly :: Transform a b -> Transform a b
contEndOnly t@(MappingTransform f) = ContTransform (\i -> ([f i], t)) []
contEndOnly IdentTransform = ContTransform (\i -> ([i], IdentTransform)) []
contEndOnly t@(ContTransform _ _) = t
contEndOnly t@(EndTransform _) = t

endTransform (bs, t) = bs ++ closeTransform t

closeTransform (EndTransform bs)    = bs
closeTransform (ContTransform _ bs) = bs
closeTransform _ = []

feedTransform :: [a] -> Transform a b -> ([b], Transform a b)
feedTransform toFeed trans = step [] toFeed trans
    where step outs []     t = (outs, t)
          step outs es   t@IdentTransform = (outs ++ es, t)
          step outs es   t@(MappingTransform f) = (outs ++ map f es, t)
          step outs (e:es) (ContTransform f _)  = let (r, t') = f e in step (outs ++ r) es t'
          step outs _    t@(EndTransform _) = (outs, t) --lefover es are lost
