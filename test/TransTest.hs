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

listSink :: Sink Int Identity [Int]
listSink = L.listSink

run = runIdentity

transList t l = run (listSource l $$ t =$ listSink)

-- take
take1Length d = length (transList (T.take 5) d) == (min 5 (length d))

take5Length d =  length (transList (T.take 5) d) == (min 5 (length d))

sameAsPreludeTake5 d = transList (T.take 5) d == take 5 d

sameAsPreludeTakeN d n = transList (T.take n) d == take n d

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

--Main
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Trans.take" [
    	testProperty "length 1" take1Length,
      	testProperty "length 5" take5Length,
      	testProperty "same as Prelude.take 5" sameAsPreludeTake5,
      	testProperty "same as Prelude.take x" sameAsPreludeTakeN
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
      ]
    ]
