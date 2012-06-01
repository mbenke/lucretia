{-# LANGUAGE CPP #-}
--module Lucretia.TypeChecker.Test(tests, main) where
module Lucretia.TypeChecker.Test where

import Test.HUnit

import qualified Data.Map as Map
import qualified Data.Set as Set

import HUnitUtils (assertEqualShowingDiff)

import Lucretia.TypeChecker.TypeChecker (checkProg, runCheck)
import Lucretia.Syntax
import Lucretia.Types

#define VARIABLE_NAME(variable) ("variable", variable)

tests :: [Test]
tests = outputTypeTests

outputTypeTestsData :: [((String, Exp), String)]
outputTypeTestsData = [
  (VARIABLE_NAME(eInt), "Right (int,[])"),
  (VARIABLE_NAME(ENew), "Right (X1,[X1 < {}])"),
  (VARIABLE_NAME(eLet), "Right (X1,[X1 < {}])"),
  -- Record update (update-old)
  (VARIABLE_NAME(eSetGet), "Right (int,[])"),
  -- Record access (access)
  (VARIABLE_NAME(eGet_noVar), "Left \"Unknown variable foo\""),
  (VARIABLE_NAME(eGet_varNotRec), "Left \"Variable foo: type mismatch: expected record type, but got bool.\""),
  (VARIABLE_NAME(eGet_wrongField), "Left \"Unknown field foo.a\""),

  (VARIABLE_NAME(eRecordWithOneField), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eRecordWithManyFields), "Right (X1,[X1 < {a:int, b:int, c:int}])"),
  (VARIABLE_NAME(eIfTOr), "Right (X1,[X1 < {a:int v bool}])"),
  (VARIABLE_NAME(eIfTFieldUndefined), "Right (X1,[X1 < {a:int v undefined, b:bool v undefined}])"),
  (VARIABLE_NAME(eIfHasAttr), "Right (int,[])"),
  (VARIABLE_NAME(eIfHasAttr_noSuchField), "Left \"Record {a:int v undefined, b:bool v undefined} does not contain field c\""),
  (VARIABLE_NAME(eFunc), "Right (([] bool int -> bool []),[])"),
  (VARIABLE_NAME(eFuncWrongReturnType), "Left \"Type and associated constraints after type-checking method body: bool, [] are not the same as declared in the signature: int, [].\\nExpected type bool but got int.\\n\""),
  (VARIABLE_NAME(eFuncWithConstraints), "Right (([] bool int -> X1 [X1 < {a:int}]),[])"),
  (VARIABLE_NAME(eFuncWrongNumberOfArguments), "Left \"Number of arguments and number of their types do not match\""),
  --maybe this should be ok:
  --(eFuncReturningRecordWithMoreFieldsThanRequiredByConstraintsInFunctionSignature, "Right (([] bool int -> X1 [X1 < {a:int}]),[])"),
  (VARIABLE_NAME(eFuncDefiningRecordInsideButNotReturningIt), "Right (([] bool int -> bool []),[])"),
  (VARIABLE_NAME(eLetDefiningRecordInsideButNotReturningIt), "Right (bool,[])"),
  (VARIABLE_NAME(eFuncDefiningNestedRecordInsideAndReturningIt), "Right (([] bool int -> X3 [X1 < {a:int}, X3 < {c:X1}]),[])"),
  (VARIABLE_NAME(eLetDefiningNestedRecordInsideAndReturningIt), "Right (X3,[X1 < {a:int}, X3 < {c:X1}])"),
  (VARIABLE_NAME(eFuncDefiningNestedRecordInsideAndReturningItHavingDifferentlyNamedConstraintVariables), "Right (([] bool int -> C [A < {a:int}, C < {c:A}]),[])"),
  (VARIABLE_NAME(eFuncWithUnnecessaryConstraints), "Right (([] bool int -> X1 [X1 < {a:int}]),[])"),
  (VARIABLE_NAME(eFuncDefiningNestedRecordInsideAndHavingMismatchingConstraintSignature1), "Left \"Type and associated constraints after type-checking method body: X3, [X1 < {a:int}, X3 < {c:X1}] are not the same as declared in the signature: C, [C < {c:int}].\\nExpected type X1 but got int.\\n\""),

  --TODO test catching error
  (VARIABLE_NAME(eFuncWithDanglingTypeVariableInSignature), "Left \"Type and associated constraints after type-checking method body: X3, [X1 < {a:int}, X3 < {c:X1}] are not the same as declared in the signature: C, [A < {a:int}].\\nCannot find type variable named: C in constraints: [A < {a:int}].\\n\""),
  (VARIABLE_NAME(eCall), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eLetReturningCyclicNestedRecord), "Right (X3,[X1 < {a:X3}, X3 < {c:X1}])"),
  (VARIABLE_NAME(eFuncReturningCyclicNestedRecord), "Right (([] bool int -> X3 [X1 < {a:X3}, X3 < {c:X1}]),[])"),
  (VARIABLE_NAME(eCallLet), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eFuncTOr), "Right (([]  -> X [X < {a:int v bool}]),[])"),
  (VARIABLE_NAME(eFuncTOrWrongConstraints), "Left \"Type and associated constraints after type-checking method body: X1, [X1 < {a:int v bool}] are not the same as declared in the signature: X, [X < {a:int}].\\nType mismatch:\\n  [int]\\n  [int,bool]\"")
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

eGet_noVar :: Exp
eGet_noVar =
  EGet "foo" "a"

eGet_varNotRec :: Exp
eGet_varNotRec =
  ELet "foo" EBoolTrue $
  EGet "foo" "a"

eGet_wrongField :: Exp
eGet_wrongField =
  ELet "foo" ENew $
  EGet "foo" "a"

eSetGet :: Exp
eSetGet =
  ELet "foo" ENew $
  ELet "_" (ESet "foo" "a" $ EInt 42) $
  EGet "foo" "a"

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

eIfHasAttr :: Exp
eIfHasAttr =
  ELet "foo" ENew $
  ELet "_"
  (EIf (EBoolTrue)
    (ESet "foo" "a" $ EInt 42)
    (ESet "foo" "b" $ EBoolTrue)
  ) $
  EIfHasAttr "foo" "a"
    (EGet "foo" "a")
    (EInt 7)

eIfHasAttr_noSuchField :: Exp
eIfHasAttr_noSuchField =
  ELet "foo" ENew $
  ELet "_"
  (EIf (EBoolTrue)
    (ESet "foo" "a" $ EInt 42)
    (ESet "foo" "b" $ EBoolTrue)
  ) $
  EIfHasAttr "foo" "c"
    (EInt 6)
    (EInt 7)

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

eFuncDefiningNestedRecordInsideAndReturningIt :: Exp
eFuncDefiningNestedRecordInsideAndReturningIt =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.empty) [TBool, TInt] (TVar "X3") (Map.fromList [("X1", oneFieldTRec "a" TInt), ("X3", oneFieldTRec "c" (TVar "X1"))])) $
    eLetDefiningNestedRecordInsideAndReturningIt
  )

eLetReturningCyclicNestedRecord :: Exp
eLetReturningCyclicNestedRecord =
  ELet "x" ENew $
  ELet "y" ENew $
  ELet "z" ENew $
  ELet "_" (ESet "x" "a" $ EVar "z") $
  ELet "_" (ESet "y" "b" $ EInt 7) $
  ELet "_" (ESet "z" "c" $ EVar "x") $
    EVar "z"

eFuncReturningCyclicNestedRecord :: Exp
eFuncReturningCyclicNestedRecord =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.empty) [TBool, TInt] (TVar "X3") (Map.fromList [("X1", oneFieldTRec "a" (TVar "X3")), ("X3", oneFieldTRec "c" (TVar "X1"))])) $
    eLetReturningCyclicNestedRecord
  )


eLetDefiningNestedRecordInsideAndReturningIt :: Exp
eLetDefiningNestedRecordInsideAndReturningIt =
  ELet "x" ENew $
  ELet "_" (ESet "x" "a" $ EInt 42) $
  ELet "y" ENew $
  ELet "_" (ESet "y" "b" $ EInt 7) $
  ELet "z" ENew $
  ELet "_" (ESet "z" "c" $ EVar "x") $
    EVar "z"

eFuncDefiningNestedRecordInsideAndReturningItHavingDifferentlyNamedConstraintVariables :: Exp
eFuncDefiningNestedRecordInsideAndReturningItHavingDifferentlyNamedConstraintVariables =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.empty) [TBool, TInt] (TVar "C") (Map.fromList [("A", oneFieldTRec "a" TInt), ("C", oneFieldTRec "c" (TVar "A"))])) $
    eLetDefiningNestedRecordInsideAndReturningIt
  )

eFuncDefiningNestedRecordInsideAndHavingMismatchingConstraintSignature1 :: Exp
eFuncDefiningNestedRecordInsideAndHavingMismatchingConstraintSignature1 =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.empty) [TBool, TInt] (TVar "C") (Map.fromList [("C", oneFieldTRec "c" TInt)])) $
    eLetDefiningNestedRecordInsideAndReturningIt
  )

eFuncWithUnnecessaryConstraints :: Exp
eFuncWithUnnecessaryConstraints =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.fromList []) [TBool, TInt] (TVar "X1") (Map.fromList [("X1", oneFieldTRec "a" TInt), ("X2", oneFieldTRec "b" TBool)]))
    eRecordWithOneField
  )

eFuncWithDanglingTypeVariableInSignature :: Exp
eFuncWithDanglingTypeVariableInSignature =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.empty) [TBool, TInt] (TVar "C") (Map.fromList [("A", oneFieldTRec "a" TInt)])) $
    eLetDefiningNestedRecordInsideAndReturningIt
  )

eFuncDefiningRecordInsideButNotReturningIt :: Exp
eFuncDefiningRecordInsideButNotReturningIt =
  EFunc (Func 
    ["x1", "x2"]
    (TFunc (Map.empty) [TBool, TInt] TBool (Map.empty)) $
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

emptyConstraints = Map.fromList []

eFuncTOr :: Exp
eFuncTOr =
  EFunc $ Func
    []
    (TFunc emptyConstraints [] (TVar "X")
      (Map.fromList [("X", oneFieldTRec "a" (TOr $ Set.fromList [TBool, TInt]))])
    ) $
    eIfTOr

eFuncTOrWrongConstraints :: Exp
eFuncTOrWrongConstraints =
  EFunc $ Func
    []
    (TFunc emptyConstraints [] (TVar "X")
      (Map.fromList [("X", oneFieldTRec "a" (TOr $ Set.fromList [TInt]))])
    ) $
    eIfTOr
