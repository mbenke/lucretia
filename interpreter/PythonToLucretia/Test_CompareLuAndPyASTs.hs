module Lucretia.Test_CompareLuAndPyASTs(main) where

import Control.Monad.Error
import Language.Python.Version3 
import qualified Language.Python.Common.Pretty as Py (pretty)
import Language.Python.Common.PrettyAST

import Diff(diff)

import PythonToLucretia.PythonToLucretia (pyToLu)
import qualified Lucretia.PrettyPrint as Lu (pretty)
import Lucretia.Syntax

{- TODO

Way of testing presented below is deprecated because python code maybe translated to a code in lucretia that not necessarily could be interpreted. The aim of translation of a programme is to preserve type-checkedness of it.

testy:
Weryfikacja: Sprawdzenie poprawności tłumaczenia PyAST -> LuAST.
diff (test.py -> PyAST -> LuAST) ({test.lu ->} LuAST)
	wrong => show 1st, 2nd tree
	         (show pretty PyAST == test.py)
	         (show pretty LuAST == test.lu)
Walidacja: Sprawdzenie poprawności wyniku (uruchomienia interpreterów)
diff (lucretiaAST (test.py -> PyAST -> LuAST)) (python test.py)
	wrong => show 1st, 2nd output
	         show LuAST, show PyAST
	         show pretty PyAST, show pretty LuAST
	         (show pretty PyAST == test.py)


Refactor: use HUnit in Test_CompareLuAndPyASTs
-}

main :: IO ()
main = do
  printPyAndLuASTs "PythonToLucretia/tests/print_01.py"

printPyAndLuASTs codeFileName = do
  code           <- readFile codeFileName
  let (Right (pyAST, _)) = parseModule code codeFileName
  let luAST = pyToLu pyAST

  putStrLn ""
  putStrLn "===================="
  print pyAST
  print . Lu.pretty $ luAST
  print . Py.pretty $ pyAST

