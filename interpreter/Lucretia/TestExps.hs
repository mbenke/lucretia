module Lucretia.TestExps where

import Lucretia.Syntax
import Lucretia.Types

eLetSthInInt = ELet "x" (EInt 42) (EInt 53)
eLetXInX     = ELet "x" (EInt 42) (EVar "x")
eLetXIn_eLetSthUsingXInInt = ELet "x" (EInt 42) $ ELet "y" (EVar "x") (EInt 53)

eIf_then_eNone_else_eLetSthInX = EIf EBoolTrue ENone $ ELet "x" (EInt 42) (EInt 53)

eIf2 = ELet "x" (EInt 42) $ EIf EBoolTrue (EInt 1) (EInt 2)
eIf3 = ELet "x" (EInt 42) $ EIf EBoolTrue (EVar "x") (EInt 2)
eIf4 = ELet "x" (EInt 42) $ EIf EBoolTrue (EInt 1) (EVar "x")

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
      EFunc $ Func ["f_functions", "n"] TInt $
	EIf (EVar "n")
	(
	  ELet "t1" (EAdd (EVar "n") (EInt $ -1)) $
	  ELet "t2" (EGet "f_functions" "f") $
	  EMul (EVar "n") $ ECall (EVar "t2") [EVar "f_functions", EVar "t1"]
	)
        (
	  EInt 1
	)
  )
  (
    ELet "t3" (EGet "functions" "f") $
    ECall (EVar "t3") [EVar "functions", EInt 5]
  )

	
      

