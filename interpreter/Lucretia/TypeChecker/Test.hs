{-# LANGUAGE TemplateHaskell #-}
module Lucretia.TypeChecker.Test where

import Test.HUnit

import qualified Data.Map as Map
import qualified Data.Set as Set

import HUnitUtils (assertEqualShowingDiff)

import Utils.VariableName (variableNameAndValue)

import Lucretia.Definitions
import Lucretia.TypeChecker.TypeChecker (runCheck)
import Lucretia.Syntax
import Lucretia.Types

tests :: [Test]
tests = outputTypeTests

type OutputTestDatum = ((String, Exp), String)
outputTypeTestsData :: [OutputTestDatum]
outputTypeTestsData = [
  ($(variableNameAndValue 'eInt42), "Right (int,[])"),
  ($(variableNameAndValue 'eNew), "Right (X1,[X1 < {}])"),
  ($(variableNameAndValue 'eLet), "Right (X1,[X1 < {}])"),
  -- Record update (update-old)
  ($(variableNameAndValue 'eSetGet), "Right (int,[])"),
  -- Record access (access)
  ($(variableNameAndValue 'eGet_noVar), "Left \"Unknown variable foo.\""),
  ($(variableNameAndValue 'eGet_varNotRec), "Left \"Variable type mismatch: expected record type, but got bool.\""),
  ($(variableNameAndValue 'eGet_wrongField), "Left \"Record {} does not contain field a\""),

  ($(variableNameAndValue 'eRecordWithOneField), "Right (X1,[X1 < {a:int}])"),
  ($(variableNameAndValue 'eRecordWithManyFields), "Right (X1,[X1 < {a:int, b:int, c:int}])"),
  ($(variableNameAndValue 'eIfTOr), "Right (X1,[X1 < {a:int v bool}])"),
  ($(variableNameAndValue 'eIfTFieldUndefined), "Right (X1,[X1 < {a:int v undefined, b:bool v undefined}])"),
  ($(variableNameAndValue 'eIfHasAttr), "Right (int,[])"),
  ($(variableNameAndValue 'eIfHasAttr_noSuchField), "Left \"Record {a:int v undefined, b:bool v undefined} does not contain field c\""),
  ($(variableNameAndValue 'eFunc), "Right ([];;bool int -> bool [],[])"),
  ($(variableNameAndValue 'eFuncWrongReturnType), "Left \"Expected condition (type: int; with constraints: []) should be weaker or equal to actual condition (type: bool; with constraints: []). They are not because: type bool does not equal int.\""),
  ($(variableNameAndValue 'eFuncWithConstraints), "Right ([];;bool int -> X1 [X1 < {a:int}],[])"),
  ($(variableNameAndValue 'eFuncWrongNumberOfArguments), "Left \"Number of arguments and number of their types do not match\""),
  --maybe this should be ok:
  --(eFuncReturningRecordWithMoreFieldsThanRequiredByConstraintsInFunctionSignature, "Right ([] bool int -> X1 [X1 < {a:int}],[])"),
  ($(variableNameAndValue 'eFuncDefiningRecordInsideButNotReturningIt), "Right ([];;bool int -> bool [],[])"),
  ($(variableNameAndValue 'eLetDefiningRecordInsideButNotReturningIt), "Right (bool,[])"),
  ($(variableNameAndValue 'eFuncDefiningNestedRecordInsideAndReturningIt), "Right ([];;bool int -> X3 [X1 < {a:int}, X3 < {c:X1}],[])"),
  ($(variableNameAndValue 'eLetDefiningNestedRecordInsideAndReturningIt), "Right (X3,[X1 < {a:int}, X3 < {c:X1}])"),
  ($(variableNameAndValue 'eFuncTOr), "Right ([];; -> X [X < {a:int v bool}],[])"),
  ($(variableNameAndValue 'eFuncTOr_withGarbageConstraint), "Right ([];; -> X [X < {a:int}],[])"),
  ($(variableNameAndValue 'eFuncTOr_postCondition_expectedWeakerThanActual), "Right ([];; -> X [X < {a:int v bool}],[])"),
  ($(variableNameAndValue 'eFuncTOr_postCondition_expectedWeakerThanActual_moreRecords), "Right ([];; -> X [X < {a:int v bool}],[])"),
  ($(variableNameAndValue 'eFuncTOr_postCondition_expectedStrongerThanActual), "Left \"Expected condition (type: X; with constraints: [X < {a:int}]) should be weaker or equal to actual condition (type: X1; with constraints: [X1 < {a:int v bool}]). They are not because: type mismatch:\\n  [int]\\n  [int,bool]\""),
  ($(variableNameAndValue 'eFuncDefiningNestedRecordInsideAndReturningItHavingDifferentlyNamedConstraintVariables), "Right ([];;bool int -> C [A < {a:int}, C < {c:A}],[])"),
  ($(variableNameAndValue 'eFuncWithUnnecessaryConstraints), "Right ([];;bool int -> X1 [X1 < {a:int}, X2 < {b:bool}],[])"),
  ($(variableNameAndValue 'eFuncDefiningNestedRecordInsideAndHavingMismatchingConstraintSignature1), "Left \"Expected condition (type: C; with constraints: [C < {c:int}]) should be weaker or equal to actual condition (type: X3; with constraints: [X1 < {a:int}, X3 < {c:X1}]). They are not because: type X1 does not equal int.\""),

  ($(variableNameAndValue 'eFuncWithDanglingTypeVariableInSignature), "Left \"Expected condition (type: C; with constraints: [A < {a:int}]) should be weaker or equal to actual condition (type: X3; with constraints: [X1 < {a:int}, X3 < {c:X1}]). They are not because: cannot find type variable named: C in constraints: [A < {a:int}].\""),
  ($(variableNameAndValue 'eCall), "Right (X1,[X1 < {a:int}])"),
  ($(variableNameAndValue 'eCall_eFuncWithWeakConstraints), "Right (X1,[X1 < {a:int}])"),
  ($(variableNameAndValue 'eLetReturningCyclicNestedRecord), "Right (X3,[X1 < {a:X3}, X3 < {c:X1}])"),
  ($(variableNameAndValue 'eFuncReturningCyclicNestedRecord), "Right ([];;bool int -> X3 [X1 < {a:X3}, X3 < {c:X1}],[])"),
  ($(variableNameAndValue 'eCallLet), "Right (X1,[X1 < {a:int}])"),
  ($(variableNameAndValue 'eAdd), "Right (int,[])"),
  ($(variableNameAndValue 'eAdd_wrong), "Left \"TypeError: unsupported operand type(s) for +: int and NoneType\""),
  ($(variableNameAndValue 'e521), "TODO"),
  ($(variableNameAndValue 'e522), "TODO"),
  ($(variableNameAndValue 'e531), "TODO"),
  ($(variableNameAndValue 'e531b), "TODO"),
  ($(variableNameAndValue 'e541), "TODO"),
  ($(variableNameAndValue 'e541b), "TODO"),
  ($(variableNameAndValue 'e541c), "TODO"),
  ($(variableNameAndValue 'e541d), "TODO"),
  ($(variableNameAndValue 'e541e), "TODO"),
  ($(variableNameAndValue 'e542), "TODO"),
  ($(variableNameAndValue 'e543), "TODO"),
  ($(variableNameAndValue 'e544), "TODO"),
  ($(variableNameAndValue 'e545), "TODO"),
  ($(variableNameAndValue 'e545), "TODO"),
  ($(variableNameAndValue 'e545b), "TODO"),
  ($(variableNameAndValue 'e), "TODO"),
  ($(variableNameAndValue 'e), "TODO"),
  ($(variableNameAndValue 'e), "TODO"),
  ($(variableNameAndValue 'e), "TODO"),
  ($(variableNameAndValue 'e), "TODO"),
  ($(variableNameAndValue 'e521), "TODO")
  ]

outputTypeTests :: [Test]
outputTypeTests = map (uncurry mapToATest) outputTypeTestsData
 where 
  mapToATest :: (String, Exp) -> String -> Test
  mapToATest (eName, e) expectedType = TestLabel eName $ TestCase $ assertEqualShowingDiff
    ("For program " ++ show e ++ ":")
    expectedType
    (show $ runCheck e)

eNew :: Exp
eNew = ENew

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

-- Use this function for interactive testing, e.g.
-- test e521
test :: Exp -> String
test e = either ("ERROR:"++) show (runCheck e)

-- Example 5.2.1; works: "(X1,[X1 < {c:string}])"
e521 = ELet "ha" eTrue $
       ELet "m"  ENew $
       __ (eif (EVar "ha")
         [ESet "m" "c" (EStr "arg")]
         [ESet "m" "c" (EStr "help")]) $
       EVar "m"

-- Example 5.2.2; works: "(X1,[X1 < {c:string v undefined}])"
e522 = ELet "ha" eTrue $
       ELet "m"  ENew $
       __ (eif (EVar "ha")
         [ESet "m" "c" (EStr "arg")]
         []) $
       EVar "m"

-- Example 5.3.1; works: "([Xs < {}] Xs int -> NoneType [Xs < {a:int}],[])"
e531 = efunc ["self","x"] [ESet "self" "a" (EVar "x")]

e531b = EFunCall f [new, 42] where
  f = efunc ["self","x"] (__ (ESet "self" "a" (EVar "x")) $ EVar "self")
-- Example 5.4.2; works: "(X1,[X1 < {self:X1}])"
e542 =
  ELet "o" new $
  __ (ESet "o" "self" (EVar"o")) $
  ELet "s" (EGet "o" "self") $
  EGet "s" "self"

-- Example 5.4.3; works: (X1,[X1 < {equals:[Xa < {w:int},Y < {r:int}];a:Xa, eq:[];;int int -> bool [];Y -> bool [], w:int}])
e543 =
  ELet "eq" (efunc ["a","b"] True) $
  ELet "a" ENew $
  ELet "_" (ESet "a" "w" 0) $
  ESet "a" "equals" $
     efunc ["y"] $
       EFunCall (EVar "eq") [EGet "a" "w",EGet "y" "r"]


-- Example 5.4.1; simplified; works
e541d =
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  ELet "_" (ESet "o" "class" (EVar "C")) $
  ELet "f" (EGet "C" "init") $
  __ (EFunCall (EVar "f") [EVar "o",42]) $
  EGet "o" "a"

-- here is what we want in the end; should be int
e541 =
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  ELet "_" (ESet "o" "class" (EVar "C")) $
  ELet "c" (EGet "o" "class") $
  ELet "f" (EGet "c" "init") $
  __ (EFunCall (EVar "f") [EVar "o",42]) $
  -- EGet "o" "class"
  EVar "o"
{-
d541a = do
  dlet "m" new
  -- de $ ESet "m" "C" new
  c <- dlet "C" new

  let init = efunc ["self","x"] [ESet "self" "a" (EVar "x")]
  dset "C" "init" init
  o <- dlet "o" new
  dset "o" "class" c
  f <- dlet "f" (EGet "C" "init")
  de $ EFunCall f [o,42]
  dget "o" "a"
 -}

-- simiplified versions; working

e541b =
  ELet "o" new $
  ELet "_" (ESet "o" "foo" 13) $
  EFunCall e531 [EVar "o", 42]

e541c =
  ELet "o" new $
  ELet "_" (ESet "o" "foo" 13) $
  ELet "_" (ESet "o" "init" e531) $
  ELet "f" (EGet "o" "init") $
  EFunCall (EVar "f") [EVar "o",42]


e541e =
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  ELet "_" (ESet "o" "class" (EVar "C")) $
  ELet "f" (EGet "C" "init") $
  EFunCall (EVar "f") [EVar "o",42]

-- Example 5.4.4
{-
Python example:
 class C(object):
  def m(self):
    return 13

def m7():
  return 7

o = C()
o.m = m7
print o.m()
#END

Method call translation

o.m(args)

ifhasattr(o,m) then o.m(args) else o.class.m(o,args)


let C = new
C.m = func(self) { 13 }
let m7 = func() { 7 }
let o = new
o.class = C
o.m = m7
let f = ifhasattr(o,m) then o.m() else o.class.m(o)

-}

-- Example 5.4.4; works: "(int,[])"
e544 =
  ELet "C" new $
  __ (ESet "C" "m" $ efunc ["self"] (EInt 13)) $
  ELet "m7"  (efunc [] (EInt 13)) $
  ELet "o" new $
  __ (ESet "o" "m" (EVar "m7")) $
  EIfHasAttr "o" "m"
    (ELet "f" (EGet "o" "m") (EFunCall (EVar "f") []))
    (ELet "f" (EGet "C" "m") (EFunCall (EVar "f") [EVar "o"]))

-- Example 5.4.5;
e545 = undefined

e545b = efunc ["n"] $ ELet "f" (EGet "m" "f") b
        where b = EFunCall (EVar "f") [EVar "n"]

-- Auxiliary

new = ENew


efunc args body = EFunDecl args $ toExp body

__ :: Exp -> Exp -> Exp
__ e = ELet "_" e

--eif :: (ToExp t, ToExp e) => Exp -> t -> e -> Exp
--eif c t e = EIf (toExp c) (toExp t) (toExp e)
eif c t e = EIf c (edo t) (edo e)

edo :: [Exp] -> Exp
edo [] = ENone
edo (e:es) = __ e (edo es)

class ToExp a where
  toExp :: a -> Exp

instance ToExp Exp where
  toExp = id

instance ToExp Bool where
  toExp = EBool

instance ToExp Integer where
  toExp = EInt

instance ToExp () where
  toExp () = ENone

instance ToExp a => ToExp [a] where
  toExp [] = ENone
  toExp (a:as) = ELet "_" (toExp a) (toExp as)
