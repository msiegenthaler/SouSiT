{-# LANGUAGE MultiParamTypeClasses #-}

module Data.SouSiT.Trans (
    -- * Transform instances
    ComplexTransformer(..),
    MappingTransformer(..),
    MappingStateTransformer(..),

    -- * Concrete tranformers
    map,
    zipWithIndex,
    take,
    takeUntil,
    takeUntilEq,
    accumulate,
    buffer
) where

import Prelude hiding (take, map, id)
import Control.Monad
import Data.SouSiT

-- | Transformation that create 0..n elements out of an input and may have state
data ComplexTransformer a b = TransCont (a -> ([b], ComplexTransformer a b)) [b]
                            | TransEnd [b]
instance Transform ComplexTransformer where
    transformSink = applyComplex

applyComplex :: Monad m => ComplexTransformer a b -> Sink b m r -> Sink a m r
applyComplex _           (SinkDone r) = SinkDone r
applyComplex (TransEnd es)       sink = SinkDone $ feedSinkList es sink >>= closeSink
applyComplex (TransCont tfn tfe) sink = SinkCont next end
    where next i = liftM (applyComplex trans') (feedSinkList es sink)
            where (es, trans') = tfn i
          end = feedSinkList tfe sink >>= closeSink

mergeComplex :: ComplexTransformer a b -> ComplexTransformer b c -> ComplexTransformer a c
mergeComplex _ (TransEnd r) = TransEnd r
mergeComplex (TransEnd r) t2 = TransEnd $ endCT $ feedToComplex r t2
mergeComplex (TransCont f d) t2 = TransCont next' done'
    where next' i = (bs, mergeComplex t1' t2')
            where (as, t1') = f i
                  (bs, t2') = feedToComplex as t2
          done' = endCT $ feedToComplex d t2

endCT (bs, t) = bs ++ closeCT t

closeCT (TransEnd bs) = bs
closeCT (TransCont _ bs) = bs

feedToComplex :: [a] -> ComplexTransformer a b -> ([b], ComplexTransformer a b)
feedToComplex es t = step [] es t
    where step outs []     t = (outs, t)
          step outs (e:es) (TransCont next done) = step (outs ++ r) es t'
                where (r, t') = next e
          step outs rest   done = (outs, done)  --rest is lost

mappingStateToComplex :: MappingStateTransformer a b -> ComplexTransformer a b
mappingStateToComplex (MappingStateTransformer f) = TransCont step []
    where step i = ([e], mappingStateToComplex t')
            where (e, t') = f i

mappingToComplex :: MappingTransformer a b -> ComplexTransformer a b
mappingToComplex (MappingTransformer f) = TransCont step []
    where step i = ([f i], TransCont step [])


-- | Transformation that treats each element seperatly
data MappingTransformer a b = MappingTransformer (a -> b)
instance Transform MappingTransformer where
    transformSink (MappingTransformer f) = applyMapping f

applyMapping :: Monad m => (a -> b) -> Sink b m r -> Sink a m r
applyMapping _ (SinkDone r) = SinkDone r
applyMapping f (SinkCont next done) = SinkCont next' done
    where next' = liftM (applyMapping f) . next . f


-- | Transformation that treats each element seperatly and may have state
data MappingStateTransformer a b = MappingStateTransformer (a -> (b, (MappingStateTransformer a b)))
instance Transform MappingStateTransformer where
    transformSink (MappingStateTransformer f) = applyMappingState f

applyMappingState :: Monad m => (a -> (b, (MappingStateTransformer a b))) -> Sink b m r -> Sink a m r
applyMappingState _ (SinkDone r) = SinkDone r
applyMappingState f (SinkCont next done) = SinkCont next' done
    where next' i = liftM (transformSink f') $ next r
            where (r, f') = f i

mergeMappingState :: MappingStateTransformer a b -> MappingStateTransformer b c -> MappingStateTransformer a c
mergeMappingState (MappingStateTransformer f1) (MappingStateTransformer f2)= MappingStateTransformer step
    where step i = (r2, mergeMappingState t1 t2)
            where (r1, t1) = f1 i
                  (r2, t2) = f2 r1

mappingToMappingState :: MappingTransformer a b -> MappingStateTransformer a b
mappingToMappingState (MappingTransformer f) = MappingStateTransformer step
    where step i = (f i, MappingStateTransformer step)


-- TransformMerger instances

instance TransformMerger ComplexTransformer ComplexTransformer ComplexTransformer where
    (=$=) = mergeComplex
instance TransformMerger ComplexTransformer MappingStateTransformer ComplexTransformer where
    a =$= b = mergeComplex a (mappingStateToComplex b)
instance TransformMerger MappingStateTransformer ComplexTransformer ComplexTransformer where
    a =$= b = mergeComplex (mappingStateToComplex a) b
instance TransformMerger ComplexTransformer MappingTransformer ComplexTransformer where
    a =$= b = mergeComplex a (mappingToComplex b)
instance TransformMerger MappingTransformer ComplexTransformer ComplexTransformer where
    a =$= b = mergeComplex (mappingToComplex a) b

instance TransformMerger MappingTransformer MappingTransformer MappingTransformer where
    (MappingTransformer f) =$= (MappingTransformer g) = MappingTransformer (g . f)

instance TransformMerger MappingStateTransformer MappingStateTransformer MappingStateTransformer where
    (=$=) = mergeMappingState

instance TransformMerger MappingStateTransformer MappingTransformer MappingStateTransformer where
    a =$= b = mergeMappingState a (mappingToMappingState b)
instance TransformMerger MappingTransformer MappingStateTransformer MappingStateTransformer where
    (=$=) = mergeMappingState . mappingToMappingState



-- | Transforms each input individually by applying the function.
map :: (a -> b) -> MappingTransformer a b
map = MappingTransformer

-- | Transforms each input to a tuple (input, index of input).
-- I.e. for "Mario": (M, 0), (a, 1), (r, 2), (i, 3), (o, 4)
zipWithIndex :: MappingStateTransformer a (a, Int)
zipWithIndex = MappingStateTransformer $ step 0
    where step nr i = ((i, nr), MappingStateTransformer $ step (succ nr))

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


-- | Accumulates all elements with the accumulator function.
accumulate :: b -> (b -> a -> b) -> ComplexTransformer a b
accumulate acc f = TransCont step [acc]
    where step i = ([], accumulate (f acc i) f)

-- | Accumulates up to n elements with the accumulator function and then releases it.
buffer :: Int -> b -> (b -> a -> b) -> ComplexTransformer a b
buffer initN initAcc f | initN < 1 = error $ "Cannot buffer " ++ show initN ++ " elements"
                       | otherwise = step initN initAcc
    where step 1 acc = TransCont next [acc]
            where next i = ([f acc i], step initN initAcc)
          step n acc = TransCont next [acc] 
            where next i = ([], step (n-1) (f acc i))







