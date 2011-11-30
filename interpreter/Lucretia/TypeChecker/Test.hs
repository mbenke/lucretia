--module Lucretia.TypeChecker.Test(tests, main) where
module Lucretia.TypeChecker.Test where

import Test.HUnit

import HUnitUtils(assertEqualShowingDiff)

import Lucretia.TypeChecker.Main(checkProg, runCheck)
import Lucretia.TypeChecker.Syntax
import Lucretia.TypeChecker.Types

eInt :: Exp
eInt = EInt 42
eBoolTrue :: Exp
eBoolTrue = EBoolTrue
eBoolFalse :: Exp
eBoolFalse = EBoolFalse

oneFieldRecord :: Exp
oneFieldRecord = 
  ELet "foo" ENew $
  (ESet "foo" "a" $ EInt 42)

typeOrExampleReturnFooA :: Exp
typeOrExampleReturnFooA =
  typeOrExample $
  EGet "foo" "a"

typeOrExample :: Exp -> Exp
typeOrExample returnExp = 
  ELet "foo" ENew $
  ELet "_"
  (EIf (EBoolTrue)
    (ESet "foo" "a" $ EInt 42)
    (ESet "foo" "a" $ EBoolTrue)
  ) $
  returnExp

typeFieldUndefinedExampleReturnFoo =
  typeFieldUndefinedExample $ 
  EVar "foo"

typeFieldUndefinedExampleTypeError =
  typeFieldUndefinedExample $ 
  EGet "foo" "a"

typeFieldUndefinedExample :: Exp -> Exp
typeFieldUndefinedExample returnExp = 
  ELet "foo" ENew $
  ELet "_"
  (EIf (EBoolTrue)
    (ESet "foo" "a" $ EInt 42)
    (ESet "foo" "b" $ EBoolTrue)
  ) $
  returnExp


tests :: [Test]
tests = typeableTests ++ outputTypeTests

typeableTestsData :: [Exp]
typeableTestsData = [eInt, eBoolTrue, eBoolFalse, oneFieldRecord]
typeableTests :: [Test]
typeableTests = map mapToATest typeableTestsData
  where mapToATest = programIsTypeable . programFromExp

programIsTypeable :: Program -> Test
programIsTypeable p = TestCase $ assertBool
  ("Program " ++ show p ++ " should be typeable")
  (checkProg p)


outputTypeTestsData :: [(Exp, String)]
outputTypeTestsData = [
  (eInt, "Right (int,[])"),
  (ENew, "Right (X1,[X1<{}])"),
  (ELet "foo" ENew (EVar "foo"), "Right (X1,[X1<{}])"),
  (oneFieldRecord, "Right (X1,[X1<{a:int}])")
  ]
outputTypeTests :: [Test]
outputTypeTests = map mapToATest outputTypeTestsData
  where mapToATest (e, s) = outputTypeMatches e s

outputTypeMatches :: Exp -> String -> Test
outputTypeMatches e expectedType = TestCase $ assertEqualShowingDiff
  ("For program " ++ show e ++ ":")
  expectedType
  (show $ runCheck e)

