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
  , ($(nv 'bSetVar_x__Get_x), "X with Constraints: [Env < {x: X}, X < int]")
  , ($(nv 'bSetVar_x_to_x), "X with Constraints: [Env < {x: X}, X < int]")
  , ($(nv 'bSetVar_xy__Get_y), "Y with Constraints: [Env < {x: X, y: Y}, X < int, Y < string]")
  , ($(nv 'bGetUndefinedVar), "Programme does not type-check to any type")
  , ($(nv 'bNew), "X with Constraints: [Env < {}, X < {}]")
  , ($(nv 'bGetAttr_noVar), "Programme does not type-check to any type")
  , ($(nv 'bGetAttr_varNotRec), "Programme does not type-check to any type")
  , ($(nv 'bSetAttr_xa), "Y with Constraints: [Env < {x: X}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xab), "A with Constraints: [A < string, Env < {x: X}, X < {a: Y, b: A}, Y < int]")
  , ($(nv 'bSetAttr_xa__Get_xa), "Y with Constraints: [Env < {x: X}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xab__Get_xb), "A with Constraints: [A < string, Env < {x: X}, X < {a: Y, b: A}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb), "Y with Constraints: [A < {b: Y}, Env < {x: X, y: A}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb__Get_yb), "Y with Constraints: [A < {b: Y}, Env < {x: X, y: A}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb__Set_xa__Get_yb), "Y with Constraints: [A < {b: Y}, E < string, Env < {x: X, y: A}, X < {a: E}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_x_to_y__Set_xa__Get_ya), "Z with Constraints: [Env < {x: X, y: X}, X < {a: Z}, Z < int]")
  , ($(nv 'bFun_identity), "Y with Constraints: [Env < {identity: Y}, Y < func (Ax) [] -> Ax []]")
  , ($(nv 'bFun_identitySetFields), "C with Constraints: [C < func (Ar, Ax, Ay) [Ar < {}] -> Ar [Ar < {a: Ax, b: Ay}], Env < {identitySetFields: C}]")
  , ($(nv 'bFun_withSignature_identity), "Y with Constraints: [Env < {identity: Y}, Y < func (X) [] -> X []]")
  , ($(nv 'bFun_withSignature_identitySetFields), "C with Constraints: [C < func (R, X, Y) [R < {}] -> R [R < {a: X, b: Y}], Env < {identitySetFields: C}]")
  , ($(nv 'bFun_withSignature_tooStrongPre), "Programme does not type-check to any type")
  , ($(nv 'bFun_withSignature_tooStrongPost), "Programme does not type-check to any type")
  , ($(nv 'bFun_identityNested), "Programme does not type-check to any type")
  , ($(nv 'bFun_withSignature_identityNested), "Y with Constraints: [Env < {identityNested: Y}, Y < func (X, Identity) [Identity < func (X) [] -> X []] -> X [Identity < func (X) [] -> X []]]")
  , ($(nv 'bCall_identity), "Z with Constraints: [Env < {i: Z, identity: Y}, Y < func (Ax) [] -> Ax [], Z < int]")
  , ($(nv 'bCall_identitySetFields), "D with Constraints: [C < func (Ar, Ax, Ay) [Ar < {}] -> Ar [Ar < {a: Ax, b: Ay}], D < {a: E, b: F}, E < int, Env < {i: E, identitySetFields: C, s: F, x: D}, F < string]")
  , ($(nv 'bCall_withSignature_identityNested), "B with Constraints: [A < func (X) [] -> X [], B < int, Env < {i: B, identity: A, identityNested: Y}, Y < func (X, Identity) [Identity < func (X) [] -> X []] -> X [Identity < func (X) [] -> X []]]")
  --, ($(nv 'bFun_recursive), "a")
  --, ($(nv 'bCall_recursive), "")
  --, ($(nv '), "C")
  ]

--bSetVar_x, bSetVar_xGet_x, bSetVar_xyGet_y, bGetUndefinedVar, bNew :: Defs

cInt = EInt 42
cString = EString "hello"

bSetVar_x =
  [ SetVar "x" cInt
  ]
bSetVar_x__Get_x =
  [ SetVar "x" cInt
  , Return $ EGetVar "x"
  ]
bSetVar_x_to_x =
  [ SetVar "x" cInt
  , SetVar "x" $ EGetVar "x"
  ]
bSetVar_xy__Get_y =
  [ SetVar "x" cInt
  , SetVar "y" cString
  , Return $ EGetVar "y"
  ]
bGetUndefinedVar = 
  [ Return $ EGetVar "x"
  ]
bNew =
  [ Return ENew
  ]
bGetAttr_noVar =
  [ Return $ EGetAttr "x" "a"
  ]
bGetAttr_varNotRec =
  [ SetVar "x" cInt
  , Return $ EGetAttr "x" "a"
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
  , Return $ EGetAttr "x" "a"
  ]
bSetAttr_xab__Get_xb =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , SetAttr "x" "b" cString
  , Return $ EGetAttr "x" "b"
  ]
bSetAttr_xa__Set_xa_to_yb =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , SetVar "y" ENew
  , SetAttr "y" "b" $ EGetAttr "x" "a"
  ]
bSetAttr_xa__Set_xa_to_yb__Get_yb =
  bSetAttr_xa__Set_xa_to_yb ++
  [ Return $ EGetAttr "y" "b"
  ]
bSetAttr_xa__Set_xa_to_yb__Set_xa__Get_yb =
  bSetAttr_xa__Set_xa_to_yb ++
  [ SetAttr "x" "a" cString
  , Return $ EGetAttr "y" "b"
  ]
bSetAttr_xa__Set_x_to_y__Set_xa__Get_ya =
  [ SetVar "x" ENew
  , SetVar "y" $ EGetVar "x"
  , SetAttr "x" "a" cInt
  , Return $ EGetAttr "y" "a"
  ]
bFun_identity =
  [ SetVar "identity" $
      EFunDef ["x"] Nothing $
      [ Return $ EGetVar "x"
      ]
  ]
bFun_identitySetFields =
  [ SetVar "identitySetFields" $
      EFunDef ["r", "x", "y"] Nothing $
      [ SetAttr "r" "a" $ EGetVar "x"
      , SetAttr "r" "b" $ EGetVar "y"
      , Return $ EGetVar "r"
      ]
  ]
bFun_withSignature_identity =
  [ SetVar "identity" $
      EFunDef ["x"]
      (Just $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty))
      [ Return $ EGetVar "x"
      ]
  ]
bFun_withSignature_identitySetFields =
  [ SetVar "identitySetFields" $
      EFunDef ["r", "x", "y"]
      (Just $ TFunSingle ["R", "X", "Y"] "R"
        (DeclaredPP $ PrePost
          (Map.fromList [ ("R", tOrEmptyRec)
                        ])
          (Map.fromList [ ("R", tOrFromTRec $ Map.fromList [ ("a", (Required, "X"))
                                                           , ("b", (Required, "Y"))
                                                           ])
                        ])
        )
      )
      [ SetAttr "r" "a" $ EGetVar "x"
      , SetAttr "r" "b" $ EGetVar "y"
      , Return $ EGetVar "r"
      ]
  ]
bFun_withSignature_tooStrongPre =
  [ SetVar "f" $
      EFunDef ["x"]
      (Just $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty))
      [ Return $ EGetAttr "x" "a"
      ]
  ]
bFun_withSignature_tooStrongPost =
  [ SetVar "identity" $
      EFunDef ["x"]
      (Just $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty (Map.singleton "X" $ tOrSingletonRec "a" "Y")))
      [ Return $ EGetVar "x"
      ]
  ]
bFun_identityNested =
  [ SetVar "identityNested" $
      EFunDef ["x", "identity"]
      Nothing
      [ Return $ EFunCall "identity" ["x"]
      ]
  ]
bFun_withSignature_identityNested =
  [ SetVar "identityNested" $
      EFunDef ["x", "identity"]
      (Just $ TFunSingle ["X", "Identity"] "X"
        (DeclaredPP $ PrePost
          (Map.fromList [ ("Identity", tOrFromTFunSingle $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty)) ])
          (Map.fromList [ ("Identity", tOrFromTFunSingle $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty)) ])
        )
      )
      [ Return $ EFunCall "identity" ["x"]
      ]
  ]
bCall_identity =
  bFun_identity ++
  [ SetVar "i" cInt
  , Return $ EFunCall "identity" ["i"]
  ]
bCall_identitySetFields =
  bFun_identitySetFields ++
  [ SetVar "x" ENew
  , SetVar "i" cInt
  , SetVar "s" cString
  , Return $ EFunCall "identitySetFields" ["x", "i", "s"]
  ]
bCall_withSignature_identityNested =
  bFun_withSignature_identityNested ++
  bFun_identity ++
  [ SetVar "i" cInt
  , Return $ EFunCall "identityNested" ["i", "identity"]
  ]
preRecursive = Map.fromList
  [ ("F", tOrFromTFunSingle $ TFunSingle ["F"] "I" InheritedPP)
  ]
postRecursive = Map.fromList
  [ ("F", tOrFromTFunSingle $ TFunSingle ["F"] "I" InheritedPP)
  , ("I", tOrPrimitive KInt)
  ]
bFun_recursive =
  [ SetVar "f" $ EFunDef ["f"]
    (Just $ TFunSingle ["F"] "I"
      (DeclaredPP $ PrePost preRecursive postRecursive)
    )
    [ Return $ EFunCall "f" ["f"]
    ]
  ]

