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

listSource :: [Int] -> BasicSource2 Identity Int
listSource = L.listSource

listSink :: Sink a Identity [a]
listSink = L.listSink

run = runIdentity

transList :: Transform t => t Int a -> [Int] -> [a]
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

--buffer
bufferDoesNotChangeElements d = concat (transList (T.buffer 3 [] listAcc) d) == d

bufferMakesBlocksWithSpecifiedSize :: [Int] -> Int -> Property
bufferMakesBlocksWithSpecifiedSize d n = n < 20 && n > 0 ==>
        (length $ takeWhile (==3) (map length l)) == (length d) `div` 3
    where l = transList (T.buffer 3 [] listAcc) d

buffersLastBlockHasNmodXElements d n = n < 20 && n > 0 ==>
        length (last l) == (length d) `mod` n
    where l = transList (T.buffer n [] listAcc) d

listAcc l = (l ++) . (:[])


--Main
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Trans.take" [
        testProperty "length 1" take1Length,
        testProperty "length 5" take5Length,
        testProperty "same as Prelude.take 5" sameAsPreludeTake5,
        testProperty "same as Prelude.take x" sameAsPreludeTakeN,
        testProperty "take from infinite source terminates" takeFromInfiniteTerminates
      ],
      testGroup "Trans.takeUntil" [
        testProperty "takeUntil true is empty" takeUntilTrueIsEmpty,
        testProperty "takeUntil false is input" takeUntilFalseIsInput,
        testProperty "takeUntil 5th element is same as take 4" takeUntil5thElementIsEqTake4
      ],
      testGroup "Trans.takeUntilEq" [
        testProperty "takeUntilEq 5th element is same as take 4" takeUntilEq5thElementIsEqTake4,
        testProperty "takeUntilEq element not in list is input" takeUntilEqElNotInListIsInput,
        testProperty "takeUntilEq first element is []" takeUntilEqFirstIsEmpty
      ],
      testGroup "Trans.map" [
        testProperty "map id does not change input" mapIdDoesNotChangeInput,
        testProperty "map behaves equal to map on list" mapBehavesTheSameAsMapOnList
      ],
      testGroup "Trans.accumulate" [
        testProperty "accumulate [] (flip (:)) returns the reversed input list as first element" accumulateConsReturnsReversedInputAsFirstElement,
        testProperty "accumulate [] listAcc returns the input list as first element" accumulateListAccReturnsInputAsFirstElement,
        testProperty "accumulate always returns a single element" accumulateAlwaysReturnsASingleElement
      ],
      testGroup "Trans.buffer" [
        testProperty "buffer does not change elements (when concat'ed)" bufferDoesNotChangeElements,
        testProperty "buffer makes blocks with specified size" bufferMakesBlocksWithSpecifiedSize,
        testProperty "buffers last block has n mod x elements" buffersLastBlockHasNmodXElements
      ]
    ]









