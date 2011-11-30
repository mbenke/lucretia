module Lucretia.Test_CompareLuAndPyASTs(main) where

import Control.Monad.Error
import Language.Python.Version3 

import Diff(diff)

import Lucretia.Syntax
import Lucretia.Interpreter
import qualified Lucretia.ApplicativeParser as Parser

import PythonToLucretia.Converter

{- TODO

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
  printPyAndLuASTs "TestByPyOutput/print.py"

printPyAndLuASTs codeFileName = do
  code           <- readFile codeFileName
  let (Right (pyAST, _)) = parseModule code codeFileName
  let luAST = convertLucreciaASTToPythonAST pyAST

  putStrLn $ ""
  putStrLn $ "===================="
  putStrLn $ show $ pyAST
  putStrLn $ show $ luAST

