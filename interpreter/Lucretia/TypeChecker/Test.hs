module Lucretia.TypeChecker.Test(tests) where

import Test.HUnit

import HUnitUtils(assertEqualShowingDiff)

import Lucretia.TypeChecker.Main(checkProg, runCheck)
import Lucretia.TypeChecker.Syntax
import Lucretia.TypeChecker.Types

tests :: [Test]
tests = typeableTests ++ outputTypeTests

typeableTestsData :: [Exp]
typeableTestsData = [ENew]
typeableTests :: [Test]
typeableTests = map mapToATest typeableTestsData
  where mapToATest = programIsTypeable . programFromExp

programIsTypeable :: Program -> Test
programIsTypeable p = TestCase $ assertBool
  ("Program " ++ show p ++ " should be typeable")
  (checkProg p)


outputTypeTestsData :: [(Exp, String)]
outputTypeTestsData = [(ENew, "Right (X1,[X1<{}])")]
outputTypeTests :: [Test]
outputTypeTests = map mapToATest outputTypeTestsData
  where mapToATest (e, s) = outputTypeMatches e s

outputTypeMatches :: Exp -> String -> Test
outputTypeMatches e expectedType = TestCase $ assertEqualShowingDiff
  ("For program " ++ show e ++ ":")
  expectedType
  (show $ runCheck e)

