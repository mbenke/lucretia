module HUnitUtils(assertIsInfixOf, assertEqualShowingDiff) where

import Control.Monad(unless)
import Data.List(isInfixOf)
import Test.HUnit
import Diff(diff)

type MessageString = String

assertIsInfixOf :: (Eq a, Show a) => MessageString -> [a] -> [a] -> Assertion
assertIsInfixOf preface needle haystack =
  unless (needle `isInfixOf` haystack) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
	   "  expected: " ++ show haystack ++ "\n" ++
           "to contain: " ++ show needle

assertEqualShowingDiff :: MessageString -> String -> String -> Assertion
assertEqualShowingDiff preface expected actual = do
  if expected == actual 
    then return () 
    else do
      diffOutput <- diff expected actual
      let msg = (if null preface then "" else preface ++ "\n") ++
  	     "diff expected actual:\n" ++
               diffOutput
      assertFailure msg
