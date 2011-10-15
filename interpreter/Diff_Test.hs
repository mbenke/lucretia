module Diff_Test where

import Test.HUnit

import Diff(diff)

main = runTestTT $ TestList [
{-
  TestCase $
    assertEqual
    "Should return diff"
    (diff "same\ndifferent" "same\ndiffrent")
    "< different
    ---
      > diffrent
      "
    ,
    -}
  TestCase $
    do
      actual <- (diff "same\ndifferent" "same\ndiffrent")
      let expected = "2c2\n< different\n---\n> diffrent\n"
      assertEqual "Should run diff" actual expected
    ]

