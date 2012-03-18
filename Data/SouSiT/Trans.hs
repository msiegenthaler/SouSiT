module Data.SouSiT.Trans (
    take,
    takeUntil,
    takeUntilEq
) where

import Prelude hiding (take)
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