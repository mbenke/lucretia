module LucretiaTest.InterpreterTest ( main, tests ) where

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types hiding ( initState )
import Lucretia.Interpreter ( Val (..), runProg, IM, runIM, printState, initState )
-- import qualified Lucretia.Parser.ParsecParser as Parser
import qualified Lucretia.Parser.ApplicativeParser as Parser

import Control.Monad.Error
import Test.HUnit

import qualified Test.Framework as F    ( Test )
import Test.Framework                   ( defaultMain, testGroup )
import Test.HUnit                       hiding ( defaultMain )

import Util.HUnit                       ( assertEqualShowingDiff
                                        , assertIsInfixOf
                                        , hUnitTestsToFrameworkTests
                                        )

main :: IO ()
main = defaultMain tests

tests :: [F.Test]
tests = hUnitTestsToFrameworkTests $ exampleTests++byValueTests++throwingErrorTests++fromFileTests

-- TODO RTR All tests in the format of: (a filename, expected output) + (a filename, expected value)

exampleTests :: [Test]
exampleTests = [
{- to show how diff is printed when outputs don't match:
  luProgramTester 
    "print(4242)"
    (expectThisOutput "4241\n")
  ,
-}
  luProgramTester 
    "print(4242)"
    (expectThisOutput "4242\n")
  ,
  luProgramTester 
    "print(4242)"
    (expectThisValue VNone)
  ,
  print42Tester $ expectThisOutput "4242\n",
  print42Tester $ expectThisValue VNone
  ]

print42Tester :: CompareResultStrategy -> Test
print42Tester = luProgramTester "print(4242)"

type LuProgramResult = (Val, String)
type CompareResultStrategy = LuProgramResult -> Assertion
type ProgramText = String

expectThisOutput :: String -> CompareResultStrategy
expectThisOutput expectedOutput (actualValue, actualOutput) =
  assertEqualShowingDiff "" expectedOutput actualOutput

expectThisValue :: Val -> CompareResultStrategy
expectThisValue expectedOutput (actualValue, actualOutput) =
  assertEqual "" expectedOutput actualValue

expectError :: CompareResultStrategy
expectError (actualValue, actualOutput) = do
  assertEqual "" actualValue VNone
  assertIsInfixOf "" "Error:" actualOutput

--withFile :: FilePath -> GetProgramTextStrategy
--withFile fileName = (fileName, readFile fileName)

luProgramTester :: ProgramText -> CompareResultStrategy -> Test
luProgramTester fileContents compareOutputStrategy =
  TestCase $ do
    putStrLn ""
    case Parser.runParser "NotNecassarilyAFile (TODO:change runParser signature)" fileContents of
      Left error -> putStr "Parse error: " >> print error
      Right program ->  do
        putStrLn $ "Parsed OK: " ++ show program
        runProg program >>= compareOutputStrategy
    putStrLn ""



byValueTestsData = [
  (text2, VInt 1),
  (text4, VNone),
  (text8, VFun ["x"] $ EVar "x"),
  (text9, VInt 42),
  (text12, VNone)
  ]

byValueTests :: [Test]
byValueTests = map mapToATest byValueTestsData
  where mapToATest (programText, expectedResult) =
          luProgramTester programText (expectThisValue expectedResult)

throwingErrorTestsData = [text10, text11]

throwingErrorTests :: [Test]
throwingErrorTests = map mapToATest throwingErrorTestsData
  where mapToATest programText = luProgramTester programText expectError


fromFileTests :: [Test]
fromFileTests = [compareInterpretedLuProgramWithOutputFile "LucretiaTest/InterpreterTest/print.lu" "LucretiaTest/InterpreterTest/print.lu.out"]
--TODO open all files in the directory

compareInterpretedLuProgramWithOutputFile :: FilePath -> FilePath -> Test
compareInterpretedLuProgramWithOutputFile programFilePath outputFilePath =
  TestCase $ do
    programText    <- readFile programFilePath
    expectedOutput <- readFile outputFilePath
    let (TestCase test) = luProgramTester programText (expectThisOutput expectedOutput)
    test
{-
TODO: Migrate the old tests below to the new way of testing.
  This would require either
    - translating ASTs (progN) to text (textN), or
    - adding possibility for testing programs by their ASTs to the new way of testing.
-}

testParserAndInterpreter :: IO ()
testParserAndInterpreter = do
  testException
  putStrLn "  .../prog2"
  runProg prog2

  putStrLn ".../prog7, expect 1"


testIM :: IM () -> IO ()
testIM m = do
  let res = runIM m initState
  case res of
    Left e -> putWordsLn ["Error:",e]
    Right (a,state) -> do
      case a of
        Left exc -> putStrLn ("Exception: "++show exc)
        Right ok -> putStrLn ("OK: "++show ok)
      printState state      

putWordsLn :: [String] -> IO ()
putWordsLn = putStrLn . unwords

testException :: IO ()
testException = do
  putStrLn "Expect ()"
  testIM $ return ()
  putStrLn "Expect Error: Expected error"
  testIM $ throwError "Expected error"
  putStrLn "Expect ()"
  testIM $
    (throwError "this error should be caught") `catchError` (\s->return ())


prog2 :: Defs
prog2 = [
  ("x", (EInt 1)),
  ("_", (ELet "x" (EInt 2) ( 
    EIf (EVar "x") (EVar "x") 42))),
  ("y" , 2),
  ("_",EVar "x"+ EVar "y")
  ]
-- undefined var
bad1 = [("_", EVar "x")]

text2 = "l=new 1; x=*l \
\ let x = 2 in if x then x else 42\
\ y = 2\
\ x+y"

text4 = "locals = new {}; \
\ locals.y=1; \
\ if locals.y then locals.y = 42 else locals.y = None;\ 
\ locals.y"

{- end of TODO -}




expr :: Exp -> Defs
expr e = [("_",e)]

func1 = EFunDecl ["x"] $ EVar "x"
-- expect id
prog8 = expr func1
text8 = "func(x) x"
-- expect 42
prog9 = expr $ EFunCall func1 [EInt 42]
text9 = "i=func(x)x;i(42)"
--text9 = "(func(x)x)(42)"

-- expect error
prog10 = expr $ EFunCall func1 [EInt 42, EInt 1]
text10 = "i=func(x)x;i(42,1)"
-- expect error
prog11 = expr $ EFunCall func1 []
text11 = "i=func(x)x;i()"

-- expect 4242
text12 = "print(4242)"

