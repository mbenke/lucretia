{-# LANGUAGE TemplateHaskell #-}
module Utils.Test where

import Utils.VariableName (variableNameAndValue, printVariableNameAndValue)

a = "hello"
main = do
  putStrLn $ show $(variableNameAndValue 'a)
  $(printVariableNameAndValue 'a)

