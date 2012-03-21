module Data.SouSiT.Trans (
    take,
    takeUntil,
    takeUntilEq,
    map
) where

import Prelude hiding (take, map)
import Control.Monad
import Data.SouSiT

-- | Takes only the first n inputs, then returns done.
take :: (Num n, Ord n) => n -> ComplexTransformer a a
take n | n > 0          = TransCont (\i -> ([i], take $ n - 1)) []
            | otherwise = TransEnd []

-- | Takes inputs until the input fullfils the predicate. The matching input is not passed on.
takeUntil :: Eq a => (a -> Bool) -> ComplexTransformer a a
takeUntil p = TransCont (step []) []
    where step sf e | p e       = (sf, TransEnd [])
                    | otherwise = ([], TransCont (step sf') sf')
                where sf' = sf ++ [e]

-- | Takes inputs until the input matches the argument. The matching input is not passed on.
takeUntilEq :: Eq a => a -> ComplexTransformer a a
takeUntilEq e = takeUntil (e ==)


-- | Transforms each input individually by applying the function.
map :: (a -> b) -> MappingTransformer a b
map = MappingTransformer

data MappingTransformer a b = MappingTransformer (a -> b)
instance Transform MappingTransformer where
    transformSink (MappingTransformer f) = applyMapping f

applyMapping :: Monad m => (a -> b) -> Sink b m r -> Sink a m r
applyMapping _ (SinkDone r) = SinkDone r
applyMapping f (SinkCont next done) = SinkCont next' done
    where next' = liftM (applyMapping f) . next . f
