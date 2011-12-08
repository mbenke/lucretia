--module Lucretia.TypeChecker.Test(tests, main) where
module Lucretia.TypeChecker.Test where

import Test.HUnit
import Data.Map(Map)
import qualified Data.Map as Map

import HUnitUtils(assertEqualShowingDiff)

import Lucretia.TypeChecker.Main(checkProg, runCheck)
import Lucretia.TypeChecker.Syntax
import Lucretia.TypeChecker.Types

tests :: [Test]
tests = outputTypeTests


outputTypeTestsData :: [(Exp, String)]
outputTypeTestsData = [
  (eInt, "Right (int,[])"),
  (ENew, "Right (X1,[X1 < {}])"),
  (ELet "foo" ENew (EVar "foo"), "Right (X1,[X1 < {}])"),
  (oneFieldRecord, "Right (X1,[X1 < {a:int}])"),
  (recordWithManyFields, "Right (X1,[X1 < {a:int, b:int, c:int}])"),
  (eIfTOr, "Right (X1,[X1 < {a:int v bool}])"),
  (eIfTFieldUndefined, "Right (X1,[X1 < {a:undefined v int, b:undefined v bool}])"),
  (eFunc, "Right (([] bool int -> bool []),[])"),
  (eFuncWrongReturnType, "Left \"Expected type: int is different than actual type: bool\""),
  (eFuncWithConstraints, "Right (([] bool int -> X1 [X1 < {a:int}]),[])"),
  (eFuncWrongNumberOfArguments, "Left \"Number of arguments and number of their types doesn't match\""){-,
  (eFuncDefiningRecordInsideButNotReturningIt, "Left"),
  (eLetDefiningRecordInsideButNotReturningIt, "Left")
  -}
  ]

outputTypeTests :: [Test]
outputTypeTests = map (uncurry mapToATest) outputTypeTestsData
 where 
  mapToATest :: Exp -> String -> Test
  mapToATest e expectedType = TestCase $ assertEqualShowingDiff
    ("For program " ++ show e ++ ":")
    expectedType
    (show $ runCheck e)


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

recordWithManyFields :: Exp
recordWithManyFields = 
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
    oneFieldRecord
  )
	
eFuncWrongNumberOfArguments :: Exp
eFuncWrongNumberOfArguments =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.fromList []) [TBool] TBool (Map.fromList [("X1", oneFieldTRec "a" TInt)]))
    oneFieldRecord
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
  ELet "_" oneFieldRecord (EBoolTrue)

