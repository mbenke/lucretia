module Lucretia.TestByOutput(main) where

import Control.Monad.Error
import Language.Python.Version3 

import Diff(diff)

import Lucretia.Syntax
import Lucretia.Test(testParser)
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
-}

main :: IO ()
main = do
  diffInterpretedLuFileAndLuOutputFile "TestByLuOutput/print.lu" "TestByLuOutput/print.lu.out"
  diffInterpretedLuFileAndLuOutputFile "TestByLuOutput/print.lu" "TestByLuOutput/print.lu.wrong.out"
  printPyAndLuASTs "TestByPyOutput/print.py"
--TODO open all files in the directory

printPyAndLuASTs codeFileName = do
  code           <- readFile codeFileName
  let (Right (pyAST, _)) = parseModule code codeFileName
  let luAST = convertLucreciaASTToPythonAST pyAST

  putStrLn $ ""
  putStrLn $ "===================="
  putStrLn $ show $ pyAST
  putStrLn $ show $ luAST

diffInterpretedLuFileAndLuOutputFile  codeFileName outputFileName = do
  code           <- readFile codeFileName
  expectedOutput <- readFile outputFileName
  let actualOutput = runLucretia codeFileName code

  printDiff codeFileName expectedOutput outputFileName actualOutput code

printDiff luFileName luExpectedOut luOutFileName luActualOut luCode = do
  if luExpectedOut == luActualOut
    then return ()
    else do
      putStrLn $ "===================="
      putStrLn $ ""

      putStrLn $ "-- diff: "++luOutFileName++", interpreter on "++luFileName++":"
      putStrLn $ "--------------------"
      d <- diff luExpectedOut luActualOut
      putStrLn $ d

      putStrLn $ "-- Interpreter on "++luFileName++", execution details:"
      putStrLn $ "--------------------"
      testParser luFileName luCode
      

runLucretia name text = case Parser.runParser name text of
  Left e -> "Parse error: "++show e
  Right luAST -> runProgGetOutput luAST

