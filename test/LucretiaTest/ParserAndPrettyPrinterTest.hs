-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Tests for the 'Lucretia.Parser' and 'Lucretia.PrettyPrint' modules.
-----------------------------------------------------------------------------

module LucretiaTest.ParserAndPrettyPrinterTest ( main, tests ) where

import Test.Framework ( Test, defaultMain )


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = []

{- TODO tests

Lu pretty printer
	test
		firstIteration = pretty (parser sourceCode)
		secondIteration = pretty (parser firstIteration)
		if (firstIteration == secondIteration) {}
		else
			diff
-}

