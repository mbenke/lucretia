module Lucretia.Test (main, tests) where
import Lucretia.Definitions
import Lucretia.Syntax
import Lucretia.Types hiding (initState)
import Lucretia.Interpreter (Val (..), runProg, IM, runIM, printState, initState)
-- import qualified Lucretia.ParsecParser as Parser
import qualified Lucretia.ApplicativeParser as Parser

import Control.Monad.Error
import Test.HUnit
import HUnitUtils (assertEqualShowingDiff, assertIsInfixOf)

luProgramTester :: ProgramText -> CompareResultStrategy -> Test

main = runTestTT $ TestList $ tests

tests :: [Test]
tests = exampleTests++byValueTests++throwingErrorTests ++ fromFileTests

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
  (text7, VInt 1),
  (text8, VFun $ Func ["x"] TInt $ EVar "x"),
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
fromFileTests = [compareInterpretedLuProgramWithOutputFile "TestByLuOutput/print.lu" "TestByLuOutput/print.lu.out"]
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

derefVar :: Name -> Exp
derefVar = EDeref . EVar

testParserAndInterpreter :: IO ()
testParserAndInterpreter = do
  testException
  putStrLn "  .../prog2"
  runProg prog2
  {-
  testParser "text2" text2
  putStrLn "  .../prog3"
  runProg prog3
  putStrLn "  .../text4, expect None"
  testParser "text4" text4
  -}
  putStrLn ".../prog5, expect 2"
  runProg prog5

  putStrLn ".../prog6, expect Break \"foo\""
  runProg prog6 
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
  ("_", (ELets [("x", 2)] ( 
    EIf (EVar "x") (EVar "x") 42))),
  ("y" , 2),
  ("_",EVar "x"+ EVar "y")
  ]
prog3 = [ 
  ("y", 
   (ELet "x" (EInt 42) (EVar "x"))),
  ("_",derefVar "y") ]
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

-- expect 2
prog5 = expr $ ELabel "return" (ELet "x" ( 1) 2)
-- expect exception "foo"
prog6 = expr $ ELabel "return" (ELet "x" (EBreak"foo" 1) 2)
-- expect 1
prog7 = expr $ ELabel "return" (ELet "x" (EBreak"return" 1) 2)
text7 = "return: let x=break return 1 in 2"

func1 = EFunc (Func ["x"] TInt (EVar "x"))
-- expect id
prog8 = expr func1
text8 = "func(x) x"
-- expect 42
prog9 = expr $ ECall func1 [EInt 42]
text9 = "i=func(x)x;i(42)"
--text9 = "(func(x)x)(42)"

-- expect error
prog10 = expr $ ECall func1 [EInt 42,EInt 1]
text10 = "i=func(x)x;i(42,1)"
-- expect error
prog11 = expr $ ECall func1 []
text11 = "i=func(x)x;i()"

-- expect 4242
text12 = "print(4242)"

