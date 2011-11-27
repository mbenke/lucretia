module Lucretia.TypeChecker.Test(tests) where

import Test.HUnit

import Lucretia.TypeChecker.Main(checkProg)
import Lucretia.TypeChecker.Syntax
import Lucretia.TypeChecker.Types

tests :: [Test]
tests = typeableTests

typeableTestsData :: [Exp]
typeableTestsData = [ENew]
typeableTests :: [Test]
typeableTests = map mapToATest typeableTestsData
  where mapToATest = programIsTypeable . programFromExp

programIsTypeable :: Program -> Test
programIsTypeable p = TestCase $ assertBool
  ("Program " ++ show p ++ " should be typeable")
  (checkProg p)

