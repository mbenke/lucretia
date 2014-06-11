-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Functions for getting the name of the variable.
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
module Util.VariableName
  ( variableNameAndValue
  , printVariableNameAndValue
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Convert a variable to a tuple of (variable name, variable value). For example: 
--
-- >    a = "hello"
-- >    $(variableNameAndValue 'a)
--
-- will result in:
--
-- >    ("a", "hello")
--
variableNameAndValue :: Name -> ExpQ
--variableNameAndValue n = [| ($(lift $ nameBase n), $(varE n)) |]
variableNameAndValue n = tupE [lift $ nameBase n, varE n]

-- | Prints out a variable for debug purposes. For example:
--
-- >    a = "hello"
-- >    $(printVariableNameAndValue 'a)
--
-- will print to the output:
--
-- >    a = "hello"
--
printVariableNameAndValue :: Name -> ExpQ
printVariableNameAndValue n = [| putStrLn ( $(lift (nameBase n ++ " = ")) ++ show $(varE n) ) |]

