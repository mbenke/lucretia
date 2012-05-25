module Lucretia.Test_RecursiveFunctions where

import Lucretia.Interpreter(runExp)
import Lucretia.TestExps(eFactorial)

main = do
  output <- runExp eFactorial
  putStrLn $ show output
