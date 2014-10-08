{-# LANGUAGE TemplateHaskell #-}
-- | HUnit tests for the 'TypeChecker' module.

module LucretiaTest.TypeCheckerTest     ( main, tests ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Test.Framework as F    ( Test )
import Test.Framework                   ( defaultMain, testGroup )
import Test.HUnit                       hiding ( defaultMain )

import Util.Debug
import Util.HUnit                       ( assertEqualShowingDiff, hUnitTestsToFrameworkTests )
import Util.VariableName                ( nv )

import Lucretia.Language.Definitions
import Lucretia.TypeChecker             ( typeProgramme )
import Lucretia.Language.Syntax
import Lucretia.Language.Types


main :: IO ()
main = defaultMain tests

tests :: [F.Test]
tests = hUnitTestsToFrameworkTests outputTypeTests

type OutputTestDatum = ((String, Defs), String)

outputTypeTests :: [Test]
outputTypeTests = map (uncurry map_to_ATest) outputTypeTestsData
 where 
  map_to_ATest :: (String, Defs) -> String -> Test
  map_to_ATest (bName, b) expectedType = TestLabel bName $ TestCase $ assertEqualShowingDiff
    ("For programme " ++ show b ++ ":")
    expectedType
    (showProgrammeType $ typeProgramme b)

outputTypeTestsData :: [OutputTestDatum]
outputTypeTestsData =
  [ ($(nv 'bSetVar_x), "X with Constraints: [Env < {x: X}, X < int]")
  , ($(nv 'bSetVar_x__Get_x), "X with Constraints: [Env < {_: X, x: X}, X < int]")
  , ($(nv 'bSetVar_x_to_x), "X with Constraints: [Env < {x: X}, X < int]")
  , ($(nv 'bSetVar_xy__Get_y), "Y with Constraints: [Env < {_: Y, x: X, y: Y}, X < int, Y < string]")
  , ($(nv 'bGetUndefinedVar), "Programme does not type-check to any type")
  , ($(nv 'bNew), "X with Constraints: [Env < {_: X}, X < {}]")
  , ($(nv 'bGetAttr_noVar), "Programme does not type-check to any type")
  , ($(nv 'bGetAttr_varNotRec), "Programme does not type-check to any type")
  , ($(nv 'bSetAttr_xa), "Y with Constraints: [Env < {x: X}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xab), "A with Constraints: [A < string, Env < {x: X}, X < {a: Y, b: A}, Y < int]")
  , ($(nv 'bSetAttr_xa__Get_xa), "Y with Constraints: [Env < {_: Y, x: X}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xab__Get_xb), "A with Constraints: [A < string, Env < {_: A, x: X}, X < {a: Y, b: A}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb), "Y with Constraints: [A < {b: Y}, Env < {x: X, y: A}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb__Get_yb), "Y with Constraints: [A < {b: Y}, Env < {_: Y, x: X, y: A}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb__Set_xa__Get_yb), "Y with Constraints: [A < {b: Y}, E < string, Env < {_: Y, x: X, y: A}, X < {a: E}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_x_to_y__Set_xa__Get_ya), "Z with Constraints: [Env < {_: Z, x: X, y: X}, X < {a: Z}, Z < int]")
  ]

--bSetVar_x, bSetVar_xGet_x, bSetVar_xyGet_y, bGetUndefinedVar, bNew :: Defs

cInt = EInt 42
cString = EString "hello"

bSetVar_x =
  [ SetVar "x" cInt
  ]
bSetVar_x__Get_x =
  [ SetVar "x" cInt
  , SetVar "_" $ EGetVar "x"
  ]
bSetVar_x_to_x =
  [ SetVar "x" cInt
  , SetVar "x" $ EGetVar "x"
  ]
bSetVar_xy__Get_y =
  [ SetVar "x" cInt
  , SetVar "y" cString
  , SetVar "_" $ EGetVar "y"
  ]
bGetUndefinedVar = 
  [ SetVar "_" $ EGetVar "x"
  ]
bNew =
  [ SetVar "_" ENew
  ]
bGetAttr_noVar =
  [ SetVar "_" $ EGetAttr "x" "a"
  ]
bGetAttr_varNotRec =
  [ SetVar "x" cInt
  , SetVar "_" $ EGetAttr "x" "a"
  ]
bSetAttr_xa =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  ]
bSetAttr_xab =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , SetAttr "x" "b" cString
  ]
bSetAttr_xa__Get_xa =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , SetVar "_" $ EGetAttr "x" "a"
  ]
bSetAttr_xab__Get_xb =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , SetAttr "x" "b" cString
  , SetVar "_" $ EGetAttr "x" "b"
  ]
bSetAttr_xa__Set_xa_to_yb =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , SetVar "y" ENew
  , SetAttr "y" "b" $ EGetAttr "x" "a"
  ]
bSetAttr_xa__Set_xa_to_yb__Get_yb =
  bSetAttr_xa__Set_xa_to_yb ++
  [ SetVar "_" $ EGetAttr "y" "b"
  ]
bSetAttr_xa__Set_xa_to_yb__Set_xa__Get_yb =
  bSetAttr_xa__Set_xa_to_yb ++
  [ SetAttr "x" "a" cString
  , SetVar "_" $ EGetAttr "y" "b"
  ]
bSetAttr_xa__Set_x_to_y__Set_xa__Get_ya =
  [ SetVar "x" ENew
  , SetVar "y" $ EGetVar "x"
  , SetAttr "x" "a" cInt
  , SetVar "_" $ EGetAttr "y" "a"
  ]

