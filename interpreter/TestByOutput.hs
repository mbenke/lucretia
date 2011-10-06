module Lucretia.TestByOutput(main) where
import Lucretia.Syntax
import Lucretia.Interpreter
-- import qualified Lucretia.ParsecParser as Parser
import qualified Lucretia.ApplicativeParser as Parser

import Control.Monad.Error

derefVar :: Name -> Exp
derefVar = EDeref . EVar

main :: IO ()
main = do
  diffInterpretedLuFileAndLuOutputFile "TestByOutput/12.lu" "TestByOutput/12.lu.out"

diffInterpretedLuFileAndLuOutputFile luFileName luOutFileName = do
  luCode	<- readFile luFileName
  luExpectedOut	<- readFile luOutFileName

  luActualOut <- testParser luFileName luCode
  putStr luExpectedOut
  --extract output from IO ()
  --compare luExpectedOut with luActualOut



testParser name text = case Parser.runParser name text of
  Left e -> putStr "Parse error: " >> print e
  Right p ->  do
    --putStrLn $ "Parsed OK: " ++ show p
    runProg p

