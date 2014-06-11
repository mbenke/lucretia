module PythonToLucretiaTest     ( main, tests ) where

import Test.Framework           ( Test, defaultMain )
import Text.Show.Pretty         ( ppShow )
import qualified Language.Python.Version3      as Py ( parseModule )
import qualified Language.Python.Common.Pretty as Py ( pretty )
import Language.Python.Common.PrettyAST

import Lucretia.Language.Syntax
import PythonToLucretia         ( pyToLu )
import qualified Lucretia.PrettyPrint as Lu ( pretty )

{- TODO tests

Weryfikacja: Sprawdzenie poprawności tłumaczenia PyAST -> LuAST.
diff (test.py -> PyAST -> LuAST) ({test.lu ->} LuAST)
	wrong => show 1st, 2nd tree
	         (show pretty PyAST == test.py)
	         (show pretty LuAST == test.lu)

Way of testing presented below is not exactly what we are looking for, because python code may possibly be translated to a code in lucretia that not necessarily can be interpreted. The aim of translation of a programme is to preserve type-checkedness of it, i.e. test.py types iff test.lu types.

Walidacja: Sprawdzenie poprawności wyniku (uruchomienia interpreterów)
diff (lucretiaAST (test.py -> PyAST -> LuAST)) (python test.py)
	wrong => show 1st, 2nd output
	         show LuAST, show PyAST
	         show pretty PyAST, show pretty LuAST
	         (show pretty PyAST == test.py)


Refactor: use HUnit in Test_CompareLuAndPyASTs
-}

--main :: IO ()
--main = defaultMain tests

tests :: [Test]
tests = []

main :: IO ()
main = do
  printPyAndLuASTs "PythonToLucretiaTest/print_01.py"

printPyAndLuASTs codeFileName = do
  code           <- readFile codeFileName
  let (Right (pyAST, _)) = Py.parseModule code codeFileName
  let luAST = pyToLu pyAST

  putStrLn ""
  putStrLn "===================="
  putStrLn . ppShow $ pyAST
  print . Lu.pretty $ luAST
  print . Py.pretty $ pyAST

