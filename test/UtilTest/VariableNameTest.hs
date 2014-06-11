-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Tests for the 'Util.VariableName' module.
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
module UtilTest.VariableNameTest                ( main, tests ) where

import Test.Framework                           ( Test, defaultMain )
import Test.Framework.Providers.QuickCheck2     ( testProperty )

import Util.VariableName                        ( variableNameAndValue )


main = defaultMain tests

a = "hello"

tests :: [Test]
tests =
  [ testProperty "variableNameAndValue" $
      $(variableNameAndValue 'a) == ("a", "hello")
  ]



