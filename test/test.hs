-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Tests for the whole "lucre" project.
-----------------------------------------------------------------------------

import Test.Framework (Test, defaultMain, testGroup)

import qualified LucretiaTest.InterpreterTest                   (tests)
import qualified LucretiaTest.ParserAndPrettyPrinterTest        (tests)
import qualified LucretiaTest.TypeCheckerTest                   (tests)
import qualified PythonToLucretiaTest                           (tests)
import qualified UtilTest.DiffTest                              (tests)
import qualified UtilTest.VariableNameTest                      (tests)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "LucretiaTest.InterpreterTest"            LucretiaTest.InterpreterTest.tests
  , testGroup "LucretiaTest.ParserAndPrettyPrinterTest" LucretiaTest.ParserAndPrettyPrinterTest.tests
  , testGroup "LucretiaTest.TypeCheckerTest"            LucretiaTest.TypeCheckerTest.tests
  , testGroup "PythonToLucretiaTest"                    PythonToLucretiaTest.tests
  , testGroup "UtilTest.DiffTest"                       UtilTest.DiffTest.tests
  , testGroup "UtilTest.VariableNameTest"               UtilTest.VariableNameTest.tests
  ]


