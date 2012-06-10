{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Main where

import Text.PrettyPrint

import PrettyPrintUtils

import Lucretia.PrettyPrint
import Lucretia.TypeChecker.Test (outputTypeTestsData, OutputTestDatum) 

import qualified Test as Test (main)

instance Pretty [OutputTestDatum] where
  pretty ts = vcat (legend : map pretty ts)
    where legend :: Doc
	  legend =
	    vcat
	    [ text "In output of programs:"
	    , indent . text $ "'Left \"ERROR_MESSAGE\"' means that checking program failed with ERROR_MESSAGE,"
	    , indent . text $ "'Right \"TYPE\"' means that checking program succeeded, typing program to TYPE."
	    ]

instance Pretty OutputTestDatum where
  pretty ((name, code), expectedOutput) =
    vcat
    [ hSpace
    , hLine
    , hSpace
    , text ("Program "++name++":")
    , (nest 2 $ pretty code)
    , (text $ "Should type to:")
    , (nest 2 $ text expectedOutput)
    ]
      

main = do
  print . pretty $ outputTypeTestsData
  print hSpace
  print hLine
  print hLine
  print hSpace
  Test.main


