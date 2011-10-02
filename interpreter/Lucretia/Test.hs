module Lucretia.Test(test) where
import Lucretia.Syntax
import Lucretia.Interpreter
-- import qualified Lucretia.ParsecParser as Parser
import qualified Lucretia.ApplicativeParser as Parser

import Control.Monad.Error

derefVar :: Name -> Exp
derefVar = EDeref . EVar

test :: IO ()
test = do
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
  -- runProg prog7 
  testParser "text7" text7
  putStrLn "\n.../prog8, expect identity fun"
  -- runProg prog8
  testParser "text8" text8
  putStrLn "\n.../prog9, expect 42"  
  -- runProg prog9
  testParser "text9" text9
  putStrLn "\n.../prog10, expect error: too many args"
  -- runProg prog10
  testParser "text10" text10
  putStrLn "\n.../prog11, expect error: not enough args"
  -- runProg prog11  
  testParser "text11" text11


testIM :: IM () -> IO ()
testIM m = do
  res <- runIM m initState
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
    
testParser name text = case Parser.runParser name text of
  Left e -> putStr "Parse error: " >> print e
  Right p ->  do
    putStrLn $ "Parsed OK: " ++ show p
    runProg p

prog2 :: Defs
prog2 = [
  ("l", ENew 1),
  ("x", EDeref (EVar "l")),
  ("_", (ELets [("x", 2)] ( 
    EIf (EVar "x") (EVar "x") 42))),
  ("y" , 2),
  ("_",EVar "x"+ EVar "y")
  ]
prog3 = [ 
  ("y", 
   (ELet "x" (ENew 42) (EVar "x"))),
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

expr :: Exp -> Defs
expr e = [("_",e)]

-- expect 2
prog5 = expr $ ELabel "return" (ELet "x" ( 1) 2)
-- expect exception "foo"
prog6 = expr $ ELabel "return" (ELet "x" (EBreak"foo" 1) 2)
-- expect 1
prog7 = expr $ ELabel "return" (ELet "x" (EBreak"return" 1) 2)
text7 = "return: let x=break return 1 in 2"

func1 = EFunc (Func ["x"] (EVar "x"))
-- expect id
prog8 = expr func1
text8 = "func(x) x"
-- expect 42
prog9 = expr $ ECall func1 [EInt 42]
--text9 = "i=func(x)x;i(42)"
text9 = "(func(x)x)(42)"

-- expect error
prog10 = expr $ ECall func1 [EInt 42,EInt 1]
text10 = "i=func(x)x;i(42,1)"
-- expect error
prog11 = expr $ ECall func1 []
text11 = "i=func(x)x;i()"