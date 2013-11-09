module Lucretia.TestExps where

import Lucretia.Syntax
import Lucretia.Types

eLetSthInInt = ELet "x" eInt42 $ EInt 53
eLetXInX     = ELet "x" eInt42 $ EVar "x"
eLetXIn_eLetSthUsingXInInt = ELet "x" eInt42 $ ELet "y" (EVar "x") (EInt 53)

eIf_then_eNone_else_eLetSthInX = EIf eTrue ENone $ ELet "x" eInt42 (EInt 53)

eIf2 = ELet "x" eInt42 $ EIf eTrue (EInt 1) (EInt 2)
eIf3 = ELet "x" eInt42 $ EIf eTrue (EVar "x") (EInt 2)
eIf4 = ELet "x" eInt42 $ EIf eTrue (EInt 1) (EVar "x")

{- PythonAST -> LuAST
   Functions

   def f(n):
     if n <= 0:
       return 1
     else:
       return n * f(n - 1)
   
   print f(5)
-}

eFactorial =
  ELet "functions" ENew $
  ELet "_"
  (
    ESet "functions" "f" $
      EFunDecl ["f_functions", "n"] $
	EIf (EVar "n")
	(
	  ELet "t1" (EAdd (EVar "n") (EInt $ -1)) $
	  ELet "t2" (EGet "f_functions" "f") $
	  EMul (EVar "n") $ EFunCall (EVar "t2") [EVar "f_functions", EVar "t1"]
	)
        (
	  EInt 1
	)
  )
  (
    ELet "t3" (EGet "functions" "f") $
    EFunCall (EVar "t3") [EVar "functions", EInt 5]
  )

	
      

