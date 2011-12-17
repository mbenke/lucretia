{-# LANGUAGE CPP #-}
--module Lucretia.TypeChecker.Test(tests, main) where
module Lucretia.TypeChecker.Test where

import Test.HUnit
import Data.Map(Map)
import qualified Data.Map as Map

import HUnitUtils(assertEqualShowingDiff)

import Lucretia.TypeChecker.Main(checkProg, runCheck)
import Lucretia.TypeChecker.Syntax
import Lucretia.TypeChecker.Types

#define VARIABLE_NAME(variable) ("variable", variable)

tests :: [Test]
tests = outputTypeTests

outputTypeTestsData :: [((String, Exp), String)]
outputTypeTestsData = [
  (VARIABLE_NAME(eInt), "Right (int,[])"),
  (VARIABLE_NAME(ENew), "Right (X1,[X1 < {}])"),
  (VARIABLE_NAME(eLet), "Right (X1,[X1 < {}])"),
  (VARIABLE_NAME(eRecordWithOneField), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eRecordWithManyFields), "Right (X1,[X1 < {a:int, b:int, c:int}])"),
  (VARIABLE_NAME(eIfTOr), "Right (X1,[X1 < {a:int v bool}])"),
  (VARIABLE_NAME(eIfTFieldUndefined), "Right (X1,[X1 < {a:undefined v int, b:undefined v bool}])"),
  (VARIABLE_NAME(eFunc), "Right (([] bool int -> bool []),[])"),
  (VARIABLE_NAME(eFuncWrongReturnType), "Left \"Expected type: int is different than actual type: bool\""),
  (VARIABLE_NAME(eFuncWithConstraints), "Right (([] bool int -> X1 [X1 < {a:int}]),[])"),
  (VARIABLE_NAME(eFuncWrongNumberOfArguments), "Left \"Number of arguments and number of their types doesn't match\""),
  --maybe this should be ok:
  --(eFuncReturningRecordWithMoreFieldsThanRequiredByConstraintsInFunctionSignature, "Right (([] bool int -> X1 [X1 < {a:int}]),[])"),
  (VARIABLE_NAME(eFuncDefiningRecordInsideButNotReturningIt), "Right (([] bool int -> bool []),[])"),
  (VARIABLE_NAME(eLetDefiningRecordInsideButNotReturningIt), "Right (bool,[])"),
  (VARIABLE_NAME(eCall), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eCallLet), "Right (X1,[X1 < {a:int}])")
  ]

outputTypeTests :: [Test]
outputTypeTests = map (uncurry mapToATest) outputTypeTestsData
 where 
  mapToATest :: (String, Exp) -> String -> Test
  mapToATest (eName, e) expectedType = TestLabel eName $ TestCase $ assertEqualShowingDiff
    ("For program " ++ show e ++ ":")
    expectedType
    (show $ runCheck e)


eInt :: Exp
eInt = EInt 42
eBoolTrue :: Exp
eBoolTrue = EBoolTrue
eBoolFalse :: Exp
eBoolFalse = EBoolFalse

eLet :: Exp
eLet = ELet "foo" ENew (EVar "foo")

eRecordWithOneField :: Exp
eRecordWithOneField = 
  ELet "foo" ENew $
  (ESet "foo" "a" $ EInt 42)

eRecordWithManyFields :: Exp
eRecordWithManyFields = 
  ELet "foo" ENew $
  ELet "_" (ESet "foo" "a" $ EInt 42) $
  ELet "_" (ESet "foo" "b" $ EInt 42) $
  ELet "_" (ESet "foo" "c" $ EInt 42) $
  EVar "foo"

eIfTOr :: Exp
eIfTOr = 
  ELet "foo" ENew $
  ELet "_"
  (EIf (EBoolTrue)
    (ESet "foo" "a" $ EInt 42)
    (ESet "foo" "a" $ EBoolTrue)
  ) $
  EVar "foo"

eIfTFieldUndefined :: Exp
eIfTFieldUndefined = 
  ELet "foo" ENew $
  ELet "_"
  (EIf (EBoolTrue)
    (ESet "foo" "a" $ EInt 42)
    (ESet "foo" "b" $ EBoolTrue)
  ) $
  EVar "foo"

eFunc :: Exp
eFunc =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.fromList []) [TBool, TInt] TBool (Map.fromList []))
    (EVar "x1")
  )

eFuncWrongReturnType :: Exp
eFuncWrongReturnType =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.fromList []) [TBool, TInt] TInt (Map.fromList []))
    (EVar "x1")
  )

eFuncWithConstraints :: Exp
eFuncWithConstraints =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.fromList []) [TBool, TInt] (TVar "X1") (Map.fromList [("X1", oneFieldTRec "a" TInt)]))
    eRecordWithOneField
  )

eFuncReturningRecordWithMoreFieldsThanRequiredByConstraintsInFunctionSignature :: Exp
eFuncReturningRecordWithMoreFieldsThanRequiredByConstraintsInFunctionSignature =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.fromList []) [TBool, TInt] (TVar "X1") (Map.fromList [("X1", oneFieldTRec "a" TInt)]))
    eRecordWithManyFields
  )

eFuncWrongNumberOfArguments :: Exp
eFuncWrongNumberOfArguments =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.fromList []) [TBool] TBool (Map.fromList [("X1", oneFieldTRec "a" TInt)]))
    eRecordWithOneField
  )

eFuncDefiningRecordInsideButNotReturningIt :: Exp
eFuncDefiningRecordInsideButNotReturningIt =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.fromList []) [TBool, TInt] TBool (Map.fromList [("X1", oneFieldTRec "a" TInt)])) $
    eLetDefiningRecordInsideButNotReturningIt
  )

eLetDefiningRecordInsideButNotReturningIt :: Exp
eLetDefiningRecordInsideButNotReturningIt =
  ELet "_" eRecordWithOneField (EBoolTrue)

eCall :: Exp
eCall =
  ECall eFuncWithConstraints [EBoolTrue, (EInt 42)]

eCallLet :: Exp
eCallLet =
  ELet "fun" eFuncWithConstraints $
    ECall (EVar "fun") [EBoolTrue, (EInt 42)]
