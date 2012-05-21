module Main (main) where

import Test.QuickCheck
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad
import Control.Monad.Identity
import Data.List (elemIndex)
import Data.SouSiT
import qualified Data.SouSiT.List as L
import qualified Data.SouSiT.Trans as T

type S a = BasicSource2 Identity a

listSource :: [a] -> BasicSource2 Identity a
listSource = L.listSource

listSink :: Sink a Identity [a]
listSink = L.listSink

run = runIdentity

transList :: Transform Int a -> [Int] -> [a]
transList t l = run (listSource l $$ t =$ listSink)

-- take
take1Length d = length (transList (T.take 5) d) == (min 5 (length d))

take5Length d =  length (transList (T.take 5) d) == (min 5 (length d))

sameAsPreludeTake5 d = transList (T.take 5) d == take 5 d

sameAsPreludeTakeN d n = transList (T.take n) d == take n d

takeFromInfiniteTerminates n = n < 100 ==> transList (T.take n) [1..] == [1..n] 

-- takeUntil
takeUntilTrueIsEmpty d = transList (T.takeUntil (\i -> True)) d == []

takeUntilFalseIsInput d = transList (T.takeUntil (\i -> False)) d == d

takeUntil5thElementIsEqTake4 d = length d > 5 ==> elemIndex nr5 d == Just 4 ==>
         transList (T.takeUntil (\i -> i == nr5)) d == take 4 d
    where nr5 = d !! 4

--takeUntilEq
takeUntilEq5thElementIsEqTake4 d = length d > 5 ==> elemIndex nr5 d == Just 4 ==>
         transList (T.takeUntilEq nr5) d == take 4 d
    where nr5 = d !! 4

takeUntilEqElNotInListIsInput d e = not (elem e d) ==> transList (T.takeUntilEq e) d == d

takeUntilEqFirstIsEmpty d = not (null d) ==> transList (T.takeUntilEq (head d)) d == []

-- id
idIsTheSameAsMapPreludeId d = transList (T.map id) d == transList (T.id) d

idDoesNotChangeInput d = transList (T.id) d == d

-- map
mapIdDoesNotChangeInput d = transList (T.map id) d == d

mapBehavesTheSameAsMapOnList d = transList (T.map (+1)) d == map (+1) d

-- accumulate
accumulateConsReturnsReversedInputAsFirstElement d = l == [reverse d]
    where l = transList (T.accumulate [] (flip (:))) d

accumulateListAccReturnsInputAsFirstElement d = l == [d]
    where l = transList (T.accumulate [] listAcc) d

accumulateAlwaysReturnsASingleElement d = length (transList (T.accumulate () noop) d) == 1
    where noop _ _ = ()

-- buffer
bufferDoesNotChangeElements d = concat (transList (T.buffer 3 [] listAcc) d) == d

bufferMakesBlocksWithSpecifiedSize :: [Int] -> Int -> Property
bufferMakesBlocksWithSpecifiedSize d n = n < 20 && n > 0 ==>
        (length $ takeWhile (==3) (map length l)) == (length d) `div` 3
    where l = transList (T.buffer 3 [] listAcc) d

buffersLastBlockHasNmodXElements d n = n < 20 && n > 0 ==>
        length (last l) == (length d) `mod` n
    where l = transList (T.buffer n [] listAcc) d

listAcc l = (l ++) . (:[])

-- zipWithIndex
zipWithIndexDoesNotChangeNumberOfElements d = length (transList T.zipWithIndex d) == length d

zipWithIndexStartsWithZero d = not (null d) ==> snd (head (transList T.zipWithIndex d)) == 0

zipWithIndexSndIs0toN d = not (null d) ==> l == [0..((length d) - 1)]
    where l = fmap snd $ transList T.zipWithIndex d


-- Merge
mergeOfTwoOfMapShouldBeSameAsSeperate = mergeCombos [T.map (*2), T.map (+1)]

mergeOfTwoOfZipWithIndexShouldBeSameAsSeperate d = mergeSameAsSeperate d T.zipWithIndex T.zipWithIndex

mergeOfMapWithZipWithIndexShouldBeSameAsSeperate d =
      mergeSameAsSeperate d (T.map (+1)) T.zipWithIndex &&
      mergeSameAsSeperate d T.zipWithIndex (T.map swap)
    where swap (a, b) = (b, a)

mergeOfMapWithTakeShouldBeSameAsSeparate d = 
      mergeSameAsSeperate d (T.map (+1)) (T.take 3) &&
      mergeSameAsSeperate d (T.take 3) (T.map (+1))

mergeOfPureShouldBeSameAsSeperate = mergeCombos [
      T.take 10, T.take 3,
      T.takeUntil (>10), T.takeUntilEq 1,
      T.accumulate 0 (+), T.accumulate 1 (*),
      T.buffer 3 0 (+)
    ]

mergeOfPureWithMappingShouldBeSameAsSeperate d =
        mergeTransTuples cmbs d && mergeTransTuples (fmap swap cmbs) d
    where cs = [T.take 10, T.take 3,
                T.takeUntil (>10), T.takeUntilEq 1,
                T.accumulate 0 (+), T.accumulate 1 (*),
                T.buffer 3 0 (+)]
          ms = [T.map (+1), T.map (*2)]
          cmbs = [ (t1, t2) | t1 <- cs, t2 <- ms ]

mergeOfPureWithZipWithIndexShouldBeSameAsSeperate d =
        mergeTransTuples cmbs d && mergeTransTuples (fmap swap cmbs) d
    where cs = [T.take 10, T.take 3]
          ms = [T.zipWithIndex]
          cmbs = [ (t1, t2) | t1 <- cs, t2 <- ms ]

swap (a, b) = (b, a)


mergeCombos ts = mergeTransTuples cmbs
    where cmbs = [ (t1, t2) | t1 <- ts, t2 <- ts]

mergeTransTuples ts d = and $ fmap (uncurry (mergeSameAsSeperate d)) ts

mergeSameAsSeperate :: Eq b => [Int] -> Transform Int a -> Transform a b -> Bool
mergeSameAsSeperate d t1 t2 = runIdentity (listSource d $$ t1 =$= t2 =$ listSink) == l2
    where l1 = runIdentity (listSource d $$ t1 =$ listSink)
          l2 = runIdentity (listSource l1 $$ t2 =$ listSink)

--drop

drop2ReducesLengthBy2 d = length (transList (T.drop 2) d) == (max 0 ((length d) - 2))

drop1ReducesLengthBy1 d = length (transList (T.drop 1) d) == (max 0 ((length d) - 1))

dropNReducesLengthByN d n = n >= 0 && n < 100 ==>
    length (transList (T.drop n) d) == (max 0 ((length d) - n))


-- dropUntil

dropUntilFalseIsEmpty d = transList (T.dropUntil (\_ -> False)) d == []

dropUntilTrueIsInput d = transList (T.dropUntil (\_ -> True)) d == d

dropUntil5thElementIsEqDrop4 d = length d > 5 ==> elemIndex nr5 d == Just 4 ==>
        transList (T.dropUntil (\i -> i == nr5)) d == drop 4 d
    where nr5 = d !! 4


-- takeWhile

takeWhileFalseIsEmpty d = transList (T.takeWhile (\_ -> False)) d == []

takeWhileTrueIsInput d = transList (T.takeWhile (\_ -> True)) d == d

takeWhileSmallerOnAscending n = n > 0 && n < 200 ==>
        transList (T.takeWhile (<n)) [1,2..] == transList (T.take (n-1)) [1,2..]

takeWhileSmallerOnOccTwice n = n > 0 && n < 99 ==>
        transList (T.takeWhile (<n)) l == transList (T.take (n-1)) l
    where l = [1..100] ++ [1..100]


-- dropWhile

dropWhileFalseIsInput d = transList (T.dropWhile (\_ -> False)) d == d

dropWhileTrueIsEmpty d = transList (T.dropWhile (\_ -> True)) d == []

dropWhileSmallerOnAscending n = n > 0 && n < 200 ==>
        transList (T.dropWhile (<n)) l == transList (T.drop (n-1)) l
    where l = [1..1000]

dropWhileSmallerOnOccTwice n = n > 0 && n < 99 ==>
        transList (T.dropWhile (<n)) l == transList (T.drop (n-1)) l
    where l = [1..100] ++ [1..100]


-- loop

loopDropTake n = n > 0 ==> n < 1000 ==>
        transList (T.loop (T.drop 1 =$= T.take 1)) [1..n] == [2,4..n]

loopTakeNEqOriginal :: [Int] -> Int -> Property
loopTakeNEqOriginal d n = n > 0 ==> n < 100 ==>
        transList (T.loop (T.take n)) d == d


-- loopN

loopNDropTake n = n > 0 ==> n < 1000 ==>
        transList (T.loopN n (T.drop 1 =$= T.take 1)) [1..] == take n [2,4..]

loopNTakeNEqOriginal :: Int -> Int -> Property
loopNTakeNEqOriginal n m = n > 0 ==> m > 0 ==> n < 100 ==> m < 100 ==>
        transList (T.loopN n (T.take m)) [1..] == take (m*n) [1..]


-- sequence
sequenceTakeNDrop1RemovesOneElement d n = n < length d ==> length d > 0 ==>
        transList (T.sequence [T.take n, T.drop 1]) d == removeAt n d
    where removeAt n xs = let (ys,zs) = splitAt n xs in ys ++ (tail zs)



--Main
main = defaultMain tests

tests :: [Test]
tests =
    [
      testGroup "Trans.id" [
        testProperty "is the same as T.map Prelude.id" idIsTheSameAsMapPreludeId,
        testProperty "does not change input" idDoesNotChangeInput
      ],
      testGroup "Trans.map" [
        testProperty "with id does not change input" mapIdDoesNotChangeInput,
        testProperty "behaves equal to map on list" mapBehavesTheSameAsMapOnList
      ],
      testGroup "Trans.zipWithIndex" [
        testProperty "does not change number of elements" zipWithIndexDoesNotChangeNumberOfElements,
        testProperty "starts with 0" zipWithIndexStartsWithZero,
        testProperty "snd is [0..N]" zipWithIndexSndIs0toN
      ],
      testGroup "Trans.take" [
        testProperty "length 1" take1Length,
        testProperty "length 5" take5Length,
        testProperty "same as Prelude.take 5" sameAsPreludeTake5,
        testProperty "same as Prelude.take x" sameAsPreludeTakeN,
        testProperty "take from infinite source terminates" takeFromInfiniteTerminates
      ],
      testGroup "Trans.takeUntil" [
        testProperty "true is empty" takeUntilTrueIsEmpty,
        testProperty "false is input" takeUntilFalseIsInput,
        testProperty "5th element is same as take 4" takeUntil5thElementIsEqTake4
      ],
      testGroup "Trans.takeUntilEq" [
        testProperty "5th element is same as take 4" takeUntilEq5thElementIsEqTake4,
        testProperty "element not in list is input" takeUntilEqElNotInListIsInput,
        testProperty "first element is []" takeUntilEqFirstIsEmpty
      ],
      testGroup "Trans.takeWhile" [
        testProperty "false is empty" takeWhileFalseIsEmpty,
        testProperty "true is input" takeWhileTrueIsInput,
        testProperty "smaller than n on [1,2..] equals take (n-1)" takeWhileSmallerOnAscending,
        testProperty "smaller than n on [1..100] ++ [1..100] equals take (n-1)" takeWhileSmallerOnOccTwice
      ],
      testGroup "Trans.accumulate" [
        testProperty "[] (flip (:)) returns the reversed input list as first element" accumulateConsReturnsReversedInputAsFirstElement,
        testProperty "[] listAcc returns the input list as first element" accumulateListAccReturnsInputAsFirstElement,
        testProperty "always returns a single element" accumulateAlwaysReturnsASingleElement
      ],
      testGroup "Trans.buffer" [
        testProperty "does not change elements (when concat'ed)" bufferDoesNotChangeElements,
        testProperty "makes blocks with specified size" bufferMakesBlocksWithSpecifiedSize,
        testProperty "last block has n mod x elements" buffersLastBlockHasNmodXElements
      ],
      testGroup "Trans.drop" [
        testProperty "2 reduces the number of elements by 2" drop2ReducesLengthBy2,
        testProperty "1 reduces the number of elements by 1" drop1ReducesLengthBy1,
        testProperty "n reduces the number of elements by n" dropNReducesLengthByN
      ],
      testGroup "Trans.dropUntil" [
        testProperty "false yields empty" dropUntilFalseIsEmpty,
        testProperty "true yields input" dropUntilTrueIsInput,
        testProperty "dropUntil 5th element is equal to drop 4" dropUntil5thElementIsEqDrop4
      ],
      testGroup "Trans.dropWhile" [
        testProperty "false is input" dropWhileFalseIsInput,
        testProperty "true is empty" dropWhileTrueIsEmpty,
        testProperty "smaller than n on [1,2..] equals drop (n-1)" dropWhileSmallerOnAscending,
        testProperty "smaller than n on [1..100] ++ [1..100] equals drop (n-1)" dropWhileSmallerOnOccTwice
      ],
      testGroup "Trans.loop" [
        testProperty "take n is the same as the original list" loopTakeNEqOriginal,
        testProperty "drop 1 >>> take 1 on [1..n] should be [2,4..n]" loopDropTake
      ],
      testGroup "Trans.loopN" [
        testProperty "take m is the same as take (m*n) of the original list" loopNTakeNEqOriginal,
        testProperty "on drop 1 >>> take 1 on [1..] should be (take n [2,4..])" loopNDropTake
      ],
      testGroup "Trans.sequence" [
        testProperty "[take n, drop 1] removes nth element" sequenceTakeNDrop1RemovesOneElement
      ],
      testGroup "Trans.=$=" [
        testProperty "of two maps should be same as seperate application" mergeOfTwoOfMapShouldBeSameAsSeperate,
        testProperty "of two zipWithIndex should be same as seperate application" mergeOfTwoOfZipWithIndexShouldBeSameAsSeperate,
        testProperty "of map and zipWithIndex should be same as seperate application" mergeOfMapWithZipWithIndexShouldBeSameAsSeperate,
        testProperty "of map with take should be same as seperate application" mergeOfMapWithTakeShouldBeSameAsSeparate,
        testProperty "of pure Ops should be same as seperate application" mergeOfPureShouldBeSameAsSeperate,
        testProperty "of pure with map should be the same as seperate application" mergeOfPureWithMappingShouldBeSameAsSeperate,
        testProperty "of pure with zipWithIndex should be the same as seperate application" mergeOfPureWithZipWithIndexShouldBeSameAsSeperate
      ]
    ]









