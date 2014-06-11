-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Tests for the 'Util.DiffTest' module.
-----------------------------------------------------------------------------

module UtilTest.DiffTest                ( main, tests ) where

import Test.Framework                   ( Test, defaultMain )
import Test.Framework.Providers.HUnit   ( testCase )
import Test.HUnit                       ( assertEqual )

import Util.Diff                        ( diff )


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testCase "diff" $ do
      actual <- (diff "same\ndifferent" "same\ndiffrent")
      let expected = "2c2\n< different\n---\n> diffrent\n"
      assertEqual "Should run diff" actual expected
  ]

