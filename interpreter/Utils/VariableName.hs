{-# LANGUAGE TemplateHaskell #-}
module Utils.VariableName (variableNameAndValue, printVariableNameAndValue) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

variableNameAndValue :: Name -> ExpQ
--variableNameAndValue n = [| ($(lift $ nameBase n), $(varE n)) |]
variableNameAndValue n = tupE [lift $ nameBase n, varE n]

printVariableNameAndValue :: Name -> ExpQ
printVariableNameAndValue n = [| putStrLn ( $(lift (nameBase n ++ " = ")) ++ show $(varE n) ) |]

