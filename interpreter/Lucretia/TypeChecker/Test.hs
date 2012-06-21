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
  (VARIABLE_NAME(eInt), "Right (int,[])"),
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
  (VARIABLE_NAME(eFunc), "Right ([] bool int -> bool [],[])"),
  (VARIABLE_NAME(eFuncWrongReturnType), "Left \"Expected condition (type: int; with constraints: []) should be weaker or equal to actual condition (type: bool; with constraints: []). They are not because: type bool does not equal int.\""),
  (VARIABLE_NAME(eFuncWithConstraints), "Right ([] bool int -> X1 [X1 < {a:int}],[])"),
  (VARIABLE_NAME(eFuncWrongNumberOfArguments), "Left \"Number of arguments and number of their types do not match\""),
  --maybe this should be ok:
  --(eFuncReturningRecordWithMoreFieldsThanRequiredByConstraintsInFunctionSignature, "Right ([] bool int -> X1 [X1 < {a:int}],[])"),
  (VARIABLE_NAME(eFuncDefiningRecordInsideButNotReturningIt), "Right ([] bool int -> bool [],[])"),
  (VARIABLE_NAME(eLetDefiningRecordInsideButNotReturningIt), "Right (bool,[])"),
  (VARIABLE_NAME(eFuncDefiningNestedRecordInsideAndReturningIt), "Right ([] bool int -> X3 [X1 < {a:int}, X3 < {c:X1}],[])"),
  (VARIABLE_NAME(eLetDefiningNestedRecordInsideAndReturningIt), "Right (X3,[X1 < {a:int}, X3 < {c:X1}])"),
  (VARIABLE_NAME(eFuncTOr), "Right ([]  -> X [X < {a:int v bool}],[])"),
  (VARIABLE_NAME(eFuncTOr_withGarbageConstraint), "Right ([]  -> X [X < {a:int}],[])"),
  (VARIABLE_NAME(eFuncTOr_postCondition_expectedWeakerThanActual), "Right ([]  -> X [X < {a:int v bool}],[])"),
  (VARIABLE_NAME(eFuncTOr_postCondition_expectedWeakerThanActual_moreRecords), "Right ([]  -> X [X < {a:int v bool}],[])"),
  (VARIABLE_NAME(eFuncTOr_postCondition_expectedStrongerThanActual), "Left \"Expected condition (type: X; with constraints: [X < {a:int}]) should be weaker or equal to actual condition (type: X1; with constraints: [X1 < {a:int v bool}]). They are not because: type mismatch:\\n  [int]\\n  [int,bool]\""),
  (VARIABLE_NAME(eFuncDefiningNestedRecordInsideAndReturningItHavingDifferentlyNamedConstraintVariables), "Right ([] bool int -> C [A < {a:int}, C < {c:A}],[])"),
  (VARIABLE_NAME(eFuncWithUnnecessaryConstraints), "Right ([] bool int -> X1 [X1 < {a:int}, X2 < {b:bool}],[])"),
  (VARIABLE_NAME(eFuncDefiningNestedRecordInsideAndHavingMismatchingConstraintSignature1), "Left \"Expected condition (type: C; with constraints: [C < {c:int}]) should be weaker or equal to actual condition (type: X3; with constraints: [X1 < {a:int}, X3 < {c:X1}]). They are not because: type X1 does not equal int.\""),

  (VARIABLE_NAME(eFuncWithDanglingTypeVariableInSignature), "Left \"Expected condition (type: C; with constraints: [A < {a:int}]) should be weaker or equal to actual condition (type: X3; with constraints: [X1 < {a:int}, X3 < {c:X1}]). They are not because: cannot find type variable named: C in constraints: [A < {a:int}].\""),
  (VARIABLE_NAME(eCall), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eCall_eFuncWithWeakConstraints), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eLetReturningCyclicNestedRecord), "Right (X3,[X1 < {a:X3}, X3 < {c:X1}])"),
  (VARIABLE_NAME(eFuncReturningCyclicNestedRecord), "Right ([] bool int -> X3 [X1 < {a:X3}, X3 < {c:X1}],[])"),
  (VARIABLE_NAME(eCallLet), "Right (X1,[X1 < {a:int}])"),
  (VARIABLE_NAME(eBreak), "Left \"Label L1 was not declared.\""),
  (VARIABLE_NAME(eLabelBreak), "Right (bool,[])"),
  (VARIABLE_NAME(eLabelBreak_preservingConstraintsDespiteApplyingSignature), "Right (X,[X < {a:int v bool}])"),
  --"Shows why $\\Psi_1$ is needed in $\\Psi_2 \\vdc \\mathfrak{f}(\\Psi'_2)$"
  (VARIABLE_NAME(eGetN), "Right (int,[])"),
  (VARIABLE_NAME(eGetN_nested), "Right (int,[])"),
  (VARIABLE_NAME(eSetN_nested), "Right (int,[])"),
  (VARIABLE_NAME(eGetN_wrong), "Left \"Unknown variable x3.\"")
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

eCall_eFuncWithWeakConstraints :: Exp
eCall_eFuncWithWeakConstraints =
  ELet "f" (EFunc (Func 
    ["x1"]
    (TFunc (Map.fromList []) [TOr $ Set.fromList [TBool, TInt]] (TVar "X1") (Map.fromList [("X1", oneFieldTRec "a" TInt)]))
    eRecordWithOneField
  )) $
  ECall (EVar "f") [EBoolTrue]

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

eFuncTOr :: Exp
eFuncTOr =
  EFunc $ Func
    []
    (TFunc emptyConstraints [] (TVar "X")
      (Map.fromList [("X", oneFieldTRec "a" (TOr $ Set.fromList [TBool, TInt]))])
    ) $
    eIfTOr

eFuncTOr_withGarbageConstraint :: Exp
eFuncTOr_withGarbageConstraint =
  EFunc $ Func
    []
    (TFunc emptyConstraints [] (TVar "X")
      (Map.fromList [("X", oneFieldTRec "a" TInt)])
    ) $
    ELet "x" ENew $
    ELet "y" ENew $
    ELet "_" (ESet "x" "a" $ EInt 7) $
    ELet "_" (ESet "y" "b" $ EInt 8) $
      EVar "x"

eFuncTOr_postCondition_expectedWeakerThanActual :: Exp
eFuncTOr_postCondition_expectedWeakerThanActual =
  EFunc $ Func
    []
    (TFunc emptyConstraints [] (TVar "X")
      (Map.fromList [("X", oneFieldTRec "a" (TOr $ Set.fromList [TBool, TInt]))])
    ) $
    eRecordWithOneField

eFuncTOr_postCondition_expectedWeakerThanActual_moreRecords :: Exp
eFuncTOr_postCondition_expectedWeakerThanActual_moreRecords =
  EFunc $ Func
    []
    (TFunc emptyConstraints [] (TVar "X")
      (Map.fromList [("X", oneFieldTRec "a" (TOr $ Set.fromList [TBool, TInt]))])
    ) $
    eRecordWithManyFields

eFuncTOr_postCondition_expectedStrongerThanActual :: Exp
eFuncTOr_postCondition_expectedStrongerThanActual =
  EFunc $ Func
    []
    (TFunc emptyConstraints [] (TVar "X")
      (Map.fromList [("X", oneFieldTRec "a" (TOr $ Set.fromList [TInt]))])
    ) $
    eIfTOr

eBreak :: Exp
eBreak =
  EBreak "L1" $
    EBoolTrue

eLabelBreak :: Exp
eLabelBreak =
  ELabel "L1"
    (TFunc emptyConstraints [] TBool emptyConstraints) $
    EBreak "L1" $
      EBoolTrue

eLabelBreak_preservingConstraintsDespiteApplyingSignature :: Exp
eLabelBreak_preservingConstraintsDespiteApplyingSignature =
  ELet "x" ENew $
  ELet "_" (ESet "x" "a" $ EInt 7) $
  ELabel "L1"
    (TFunc emptyConstraints [] (TVar "X")
      (Map.fromList [("X", oneFieldTRec "a" (TOr $ Set.fromList [TBool, TInt]))])
    ) $
    ELet "y" ENew $
    ELet "_" (ESet "y" "b" $ EInt 8) $
    EIf EBoolTrue
      (EBreak "L1" $
        ELet "_" (ESet "y" "b" $ EBoolTrue) $
	  EVar "y"
      )
      (EVar "y")

eGetN :: Exp
eGetN =
  ELet "foo" ENew $
  ELet "_" (ESet "foo" "a" $ EInt 42) $
  EGetN ["foo", "a"]

eGetN_nested :: Exp
eGetN_nested =
  ELet "x1" ENew $
  ELet "t2" ENew $
  ELet "_" (ESet "t2" "x3" $ EInt 42) $
  ELet "_" (ESet "x1" "x2" $ EVar "t2") $
  EGetN ["x1", "x2", "x3"]

eSetN_nested :: Exp
eSetN_nested =
  ELet "x1" ENew $
  ELet "_" (ESetN ["x1", "x2"] ENew) $
  ELet "_" (ESetN ["x1", "x2", "x3"] $ EInt 42) $
  EGetN ["x1", "x2", "x3"]

eGetN_wrong :: Exp
eGetN_wrong =
  ELet "x1" ENew $
  ELet "_" (ESetN ["x1", "x2"] ENew) $
  EGetN ["x1", "x2", "x3"]

