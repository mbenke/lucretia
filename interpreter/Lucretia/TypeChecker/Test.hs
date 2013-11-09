{-# LANGUAGE CPP #-}
--module Lucretia.TypeChecker.Test(tests, main) where
module Lucretia.TypeChecker.Test where

import Test.HUnit

import qualified Data.Map as Map
import qualified Data.Set as Set

import HUnitUtils (assertEqualShowingDiff)

import Lucretia.Definitions
import Lucretia.TypeChecker.TypeChecker (runCheck)
import Lucretia.Syntax
import Lucretia.Types

#define VARIABLE_NAME(variable) ("variable", variable)

tests :: [Test]
tests = outputTypeTests

type OutputTestDatum = ((String, Exp), String)
outputTypeTestsData :: [OutputTestDatum]
outputTypeTestsData = [
  (VARIABLE_NAME(eInt42), "Right (int,[])"),
  (VARIABLE_NAME(ENew), "Right (X1,[X1 < {}])"),
  (VARIABLE_NAME(eLet), "Right (X1,[X1 < {}])"),
  -- Record update (update-old)
  (VARIABLE_NAME(eSetGet), "Right (int,[])"),
  -- Record access (access)
  (VARIABLE_NAME(eGet_noVar), "Left \"Unknown variable foo.\""),
  (VARIABLE_NAME(eGet_varNotRec), "Left \"Variable type mismatch: expected record type, but got bool.\""),
  (VARIABLE_NAME(eGet_wrongField), "Left \"Record {} does not contain field a\""),

  (VARIABLE_NAME(eRecordWithOneField), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eRecordWithManyFields), "Right (X1,[X1 < {a:int, b:int, c:int}])"),
  (VARIABLE_NAME(eIfTOr), "Right (X1,[X1 < {a:int v bool}])"),
  (VARIABLE_NAME(eIfTFieldUndefined), "Right (X1,[X1 < {a:int v undefined, b:bool v undefined}])"),
  (VARIABLE_NAME(eIfHasAttr), "Right (int,[])"),
  (VARIABLE_NAME(eIfHasAttr_noSuchField), "Left \"Record {a:int v undefined, b:bool v undefined} does not contain field c\""),
  (VARIABLE_NAME(eFunc), "Right ([];;bool int -> bool [],[])"),
  (VARIABLE_NAME(eFuncWrongReturnType), "Left \"Expected condition (type: int; with constraints: []) should be weaker or equal to actual condition (type: bool; with constraints: []). They are not because: type bool does not equal int.\""),
  (VARIABLE_NAME(eFuncWithConstraints), "Right ([];;bool int -> X1 [X1 < {a:int}],[])"),
  (VARIABLE_NAME(eFuncWrongNumberOfArguments), "Left \"Number of arguments and number of their types do not match\""),
  --maybe this should be ok:
  --(eFuncReturningRecordWithMoreFieldsThanRequiredByConstraintsInFunctionSignature, "Right ([] bool int -> X1 [X1 < {a:int}],[])"),
  (VARIABLE_NAME(eFuncDefiningRecordInsideButNotReturningIt), "Right ([];;bool int -> bool [],[])"),
  (VARIABLE_NAME(eLetDefiningRecordInsideButNotReturningIt), "Right (bool,[])"),
  (VARIABLE_NAME(eFuncDefiningNestedRecordInsideAndReturningIt), "Right ([];;bool int -> X3 [X1 < {a:int}, X3 < {c:X1}],[])"),
  (VARIABLE_NAME(eLetDefiningNestedRecordInsideAndReturningIt), "Right (X3,[X1 < {a:int}, X3 < {c:X1}])"),
  (VARIABLE_NAME(eFuncTOr), "Right ([];; -> X [X < {a:int v bool}],[])"),
  (VARIABLE_NAME(eFuncTOr_withGarbageConstraint), "Right ([];; -> X [X < {a:int}],[])"),
  (VARIABLE_NAME(eFuncTOr_postCondition_expectedWeakerThanActual), "Right ([];; -> X [X < {a:int v bool}],[])"),
  (VARIABLE_NAME(eFuncTOr_postCondition_expectedWeakerThanActual_moreRecords), "Right ([];; -> X [X < {a:int v bool}],[])"),
  (VARIABLE_NAME(eFuncTOr_postCondition_expectedStrongerThanActual), "Left \"Expected condition (type: X; with constraints: [X < {a:int}]) should be weaker or equal to actual condition (type: X1; with constraints: [X1 < {a:int v bool}]). They are not because: type mismatch:\\n  [int]\\n  [int,bool]\""),
  (VARIABLE_NAME(eFuncDefiningNestedRecordInsideAndReturningItHavingDifferentlyNamedConstraintVariables), "Right ([];;bool int -> C [A < {a:int}, C < {c:A}],[])"),
  (VARIABLE_NAME(eFuncWithUnnecessaryConstraints), "Right ([];;bool int -> X1 [X1 < {a:int}, X2 < {b:bool}],[])"),
  (VARIABLE_NAME(eFuncDefiningNestedRecordInsideAndHavingMismatchingConstraintSignature1), "Left \"Expected condition (type: C; with constraints: [C < {c:int}]) should be weaker or equal to actual condition (type: X3; with constraints: [X1 < {a:int}, X3 < {c:X1}]). They are not because: type X1 does not equal int.\""),

  (VARIABLE_NAME(eFuncWithDanglingTypeVariableInSignature), "Left \"Expected condition (type: C; with constraints: [A < {a:int}]) should be weaker or equal to actual condition (type: X3; with constraints: [X1 < {a:int}, X3 < {c:X1}]). They are not because: cannot find type variable named: C in constraints: [A < {a:int}].\""),
  (VARIABLE_NAME(eCall), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eCall_eFuncWithWeakConstraints), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eLetReturningCyclicNestedRecord), "Right (X3,[X1 < {a:X3}, X3 < {c:X1}])"),
  (VARIABLE_NAME(eFuncReturningCyclicNestedRecord), "Right ([];;bool int -> X3 [X1 < {a:X3}, X3 < {c:X1}],[])"),
  (VARIABLE_NAME(eCallLet), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eAdd), "Right (int,[])"),
  (VARIABLE_NAME(eAdd_wrong), "Left \"TypeError: unsupported operand type(s) for +: int and NoneType\"")
  ]

outputTypeTests :: [Test]
outputTypeTests = map (uncurry mapToATest) outputTypeTestsData
 where 
  mapToATest :: (String, Exp) -> String -> Test
  mapToATest (eName, e) expectedType = TestLabel eName $ TestCase $ assertEqualShowingDiff
    ("For program " ++ show e ++ ":")
    expectedType
    (show $ runCheck e)

eInt42 :: Exp
eInt42 = EInt 42

eTrue :: Exp
eTrue = EBool True

eFalse :: Exp
eFalse = EBool False

eGet_noVar :: Exp
eGet_noVar =
  EGet "foo" "a"

eGet_varNotRec :: Exp
eGet_varNotRec =
  ELet "foo" eTrue $
  EGet "foo" "a"

eGet_wrongField :: Exp
eGet_wrongField =
  ELet "foo" ENew $
  EGet "foo" "a"

eSetGet :: Exp
eSetGet =
  ELet "foo" ENew $
  ELet "_" (ESet "foo" "a" eInt42) $
  EGet "foo" "a"

eLet :: Exp
eLet = ELet "foo" ENew (EVar "foo")

eRecordWithOneField :: Exp
eRecordWithOneField = 
  ELet "foo" ENew $
  ESet "foo" "a" eInt42

eRecordWithManyFields :: Exp
eRecordWithManyFields = 
  ELet "foo" ENew $
  ELet "_" (ESet "foo" "a" eInt42) $
  ELet "_" (ESet "foo" "b" eInt42) $
  ELet "_" (ESet "foo" "c" eInt42) $
  EVar "foo"

eIfTOr :: Exp
eIfTOr = 
  ELet "foo" ENew $
  ELet "_"
  (EIf eTrue
    (ESet "foo" "a" eInt42)
    (ESet "foo" "a" $ eTrue)
  ) $
  EVar "foo"

eIfTFieldUndefined :: Exp
eIfTFieldUndefined = 
  ELet "foo" ENew $
  ELet "_"
  (EIf eTrue
    (ESet "foo" "a" eInt42)
    (ESet "foo" "b" $ eTrue)
  ) $
  EVar "foo"

eIfHasAttr :: Exp
eIfHasAttr =
  ELet "foo" ENew $
  ELet "_"
  (EIf eTrue
    (ESet "foo" "a" eInt42)
    (ESet "foo" "b" $ eTrue)
  ) $
  EIfHasAttr "foo" "a"
    (EGet "foo" "a")
    (EInt 7)

eIfHasAttr_noSuchField :: Exp
eIfHasAttr_noSuchField =
  ELet "foo" ENew $
  ELet "_"
  (EIf eTrue
    (ESet "foo" "a" eInt42)
    (ESet "foo" "b" $ eTrue)
  ) $
  EIfHasAttr "foo" "c"
    (EInt 6)
    (EInt 7)

eFunc :: Exp
eFunc =
  EFunDecl
    ["x1", "x2"]
    (EVar "x1")

eFuncWrongReturnType :: Exp
eFuncWrongReturnType =
  EFunDecl
    ["x1", "x2"]
    (EVar "x1")

eFuncWithConstraints :: Exp
eFuncWithConstraints =
  EFunDecl
    ["x1", "x2"]
    eRecordWithOneField

eCall_eFuncWithWeakConstraints :: Exp
eCall_eFuncWithWeakConstraints =
  ELet "f" (EFunDecl
    ["x1"]
    eRecordWithOneField
  ) $
  EFunCall (EVar "f") [eTrue]

eFuncReturningRecordWithMoreFieldsThanRequiredByConstraintsInFunctionSignature :: Exp
eFuncReturningRecordWithMoreFieldsThanRequiredByConstraintsInFunctionSignature =
  EFunDecl
    ["x1", "x2"]
    eRecordWithManyFields

eFuncWrongNumberOfArguments :: Exp
eFuncWrongNumberOfArguments =
  EFunDecl
    ["x1", "x2"]
    eRecordWithOneField

eFuncDefiningNestedRecordInsideAndReturningIt :: Exp
eFuncDefiningNestedRecordInsideAndReturningIt =
  EFunDecl
    ["x1", "x2"]
    eLetDefiningNestedRecordInsideAndReturningIt

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
  EFunDecl
    ["x1", "x2"]
    eLetReturningCyclicNestedRecord


eLetDefiningNestedRecordInsideAndReturningIt :: Exp
eLetDefiningNestedRecordInsideAndReturningIt =
  ELet "x" ENew $
  ELet "_" (ESet "x" "a" eInt42) $
  ELet "y" ENew $
  ELet "_" (ESet "y" "b" $ EInt 7) $
  ELet "z" ENew $
  ELet "_" (ESet "z" "c" $ EVar "x") $
    EVar "z"

eFuncDefiningNestedRecordInsideAndReturningItHavingDifferentlyNamedConstraintVariables :: Exp
eFuncDefiningNestedRecordInsideAndReturningItHavingDifferentlyNamedConstraintVariables =
  EFunDecl
    ["x1", "x2"]
    eLetDefiningNestedRecordInsideAndReturningIt

eFuncDefiningNestedRecordInsideAndHavingMismatchingConstraintSignature1 :: Exp
eFuncDefiningNestedRecordInsideAndHavingMismatchingConstraintSignature1 =
  EFunDecl
    ["x1", "x2"]
    eLetDefiningNestedRecordInsideAndReturningIt

eFuncWithUnnecessaryConstraints :: Exp
eFuncWithUnnecessaryConstraints =
  EFunDecl
    ["x1", "x2"]
    eRecordWithOneField

eFuncWithDanglingTypeVariableInSignature :: Exp
eFuncWithDanglingTypeVariableInSignature =
  EFunDecl
    ["x1", "x2"]
    eLetDefiningNestedRecordInsideAndReturningIt

eFuncDefiningRecordInsideButNotReturningIt :: Exp
eFuncDefiningRecordInsideButNotReturningIt =
  EFunDecl
    ["x1", "x2"]
    eLetDefiningRecordInsideButNotReturningIt

eLetDefiningRecordInsideButNotReturningIt :: Exp
eLetDefiningRecordInsideButNotReturningIt =
  ELet "_" eRecordWithOneField $ eTrue

eCall :: Exp
eCall =
  EFunCall eFuncWithConstraints [eTrue, eInt42]

eCallLet :: Exp
eCallLet =
  ELet "fun" eFuncWithConstraints $
    EFunCall (EVar "fun") [eTrue, eInt42]

eFuncTOr :: Exp
eFuncTOr =
  EFunDecl
    []
    eIfTOr

eFuncTOr_withGarbageConstraint :: Exp
eFuncTOr_withGarbageConstraint =
  EFunDecl
    [] $
    ELet "x" ENew $
    ELet "y" ENew $
    ELet "_" (ESet "x" "a" $ EInt 7) $
    ELet "_" (ESet "y" "b" $ EInt 8) $
      EVar "x"

eFuncTOr_postCondition_expectedWeakerThanActual :: Exp
eFuncTOr_postCondition_expectedWeakerThanActual =
  EFunDecl
    []
    eRecordWithOneField

eFuncTOr_postCondition_expectedWeakerThanActual_moreRecords :: Exp
eFuncTOr_postCondition_expectedWeakerThanActual_moreRecords =
  EFunDecl
    []
    eRecordWithManyFields

eFuncTOr_postCondition_expectedStrongerThanActual :: Exp
eFuncTOr_postCondition_expectedStrongerThanActual =
  EFunDecl
    []
    eIfTOr

eAdd :: Exp
eAdd =
  EAdd (EInt 35) (EInt 7)

eAdd_wrong :: Exp
eAdd_wrong =
  EAdd (EInt 35) ENone

