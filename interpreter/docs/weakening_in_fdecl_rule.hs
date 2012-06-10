-- Following programs would not type without the amendment in fdecl rule:

----------------------------------------
----------------------------------------

-- Because of lack of a renaming of type variables - monomorphism:

----------------------------------------

Program eFuncTOr:
  func () :: []  -> X [X < {a:int v bool}]
    let foo = new in
    let _ = if True then
              foo.a = 42
            else
              foo.a = True in
    foo
Should type to:
  Right (([]  -> X [X < {a:int v bool}]),[])
Explanation:
  'new' creates empty record with name X1. This is different then name X in function signature. 

----------------------------------------

Program eFuncDefiningNestedRecordInsideAndReturningItHavingDifferentlyNamedConstraintVariables:
  func (x1, x2) :: [] bool int -> C [A < {a:int}, C < {c:A}]
    let x = new in
    let _ = x.a = 42 in
    let y = new in
    let _ = y.b = 7 in
    let z = new in
    let _ = z.c = x in
    z
Should type to:
  Right (([] bool int -> C [A < {a:int}, C < {c:A}]),[])
Explanation:
  The same situation as in the program above.

----------------------------------------
----------------------------------------

-- Because of not taking into account that actual postconditions can be stronger than expected:

----------------------------------------

Program eFuncTOr_postCondition_expectedWeakerThanActual:
  func () :: []  -> X [X < {a:int v bool}]
    let foo = new in
    foo.a = 42
Should type to:
  Right (([]  -> X [X < {a:int v bool}]),[])

----------------------------------------

Program eFuncTOr_postCondition_expectedWeakerThanActual_moreRecords:
  func () :: []  -> X [X < {a:int v bool}]
    let foo = new in
    let _ = foo.a = 42 in
    let _ = foo.b = 42 in
    let _ = foo.c = 42 in
    foo
Should type to:
  Right (([]  -> X [X < {a:int v bool}]),[])

----------------------------------------
----------------------------------------

-- Because the expected output type does not include garbage Constraints, Y < {b:int} in this case:

----------------------------------------

Program eFuncTOr_withGarbageConstraint:
  func () :: []  -> X [X < {a:int}]
    let x = new in
    let y = new in
    let _ = x.a = 7 in
    let _ = y.b = 8 in
    x
Should type to:
  Right ([]  -> X [X < {a:int}],[])

----------------------------------------
