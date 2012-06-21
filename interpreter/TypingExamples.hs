{-# LANGUAGE FlexibleInstances #-}

import Lucretia.Definitions
import Lucretia.TypeChecker.TypeChecker (runCheck,checkProg)
import Lucretia.Syntax
import Lucretia.Types
import qualified Data.Map as M

edo :: [Exp] -> Exp
edo [] = ENone
edo (e:es) = __ e (edo es)

-- Example 5.2.1; works: "(X1,[X1 < {c:string}])"
e521 = ELet "ha" EBoolTrue $
       ELet "m"  ENew $
       __ (eif (EVar "ha")
         [ESet "m" "c" (EStr "arg")]
         [ESet "m" "c" (EStr "help")]) $
       EVar "m" 
       
-- Example 5.2.2; works: "(X1,[X1 < {c:string v undefined}])"
e522 = ELet "ha" EBoolTrue $
       ELet "m"  ENew $
       __ (eif (EVar "ha")
         [ESet "m" "c" (EStr "arg")]
         []) $
       EVar "m"

-- Example 5.3.1; works: "([Xs < {}] Xs int -> NoneType [Xs < {a:int}],[])"
t531 = tfunc [("Xs",emptyRecType)] [TVar "Xs",TInt] TNone 
               [("Xs",oneFieldTRec "a" TInt)]
e531 = efunc ["self","x"] t531 [ESet "self" "a" (EVar "x")]


-- Example 5.4.2; works: "(X1,[X1 < {self:X1}])"
e542 = 
  ELet "o" new $
  __ (ESet "o" "self" (EVar"o")) $
  ELet "s" (EGet "o" "self") $
  EGet "s" "self"

-- Example 5.4.3; fails with: *** Exception: Map.find: element not in the map
te543 = ([TVar "Y"],[("Xa",oneFieldTRec "w" TInt),("Y",oneFieldTRec "r" TInt)]) 
     ==> (TBool,[])

e543 = 
  ELet "eq" ( efunc ["a","b"] teq True) $
  ELet "a" ENew $ 
  ELet "_" (ESet "a" "w" 0) $
  ESet "a" "equals" $ 
     efunc ["y"] te543 
       (ECall (EVar "eq") [EGet "a" "w",EGet "y" "r"]) 
  where
    teq = (([TInt,TInt],[]) ==> (TBool,[]))  


-- Example 5.4.1; simplified
-- FAIL "ERROR:Expected condition (type: [Xs,int]; with constraints: [Xs < {}]) should be weaker or equal to actual condition (type: [X2,int]; with constraints: [X1 < {init:[Xs < {}] Xs int -> NoneType [Xs < {a:int}]}, X2 < {class:X1}]). They are not because: Expected record: [] should be weaker (have more fields) than actual record [(\"class\",X1)]."
e541d = 
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  ELet "_" (ESet "o" "class" (EVar "C")) $
  ELet "f" (EGet "C" "init") $
  (ECall (EVar "f") [EVar "o",42])
  
-- here is what we want in the end; should be int
e541 = 
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  ELet "_" (ESet "o" "class" (EVar "C")) $
  ELet "f" (EGet "C" "init") $
  __ (ECall (EVar "f") [EVar "o",42]) $
  EGet "o" "a"
{-
d541a = do
  dlet "m" new
  -- de $ ESet "m" "C" new
  c <- dlet "C" new
  
  let tinit = tfunc  [("Xs",emptyRecType)] [TVar "Xs",TInt]
                 TNone [("Xs",oneFieldTRec "a" TInt)]
  let init = efunc ["self","x"] tinit [ESet "self" "a" (EVar "x")]
  dset "C" "init" init
  o <- dlet "o" new
  dset "o" "class" c
  f <- dlet "f" (EGet "C" "init")
  de $ ECall f [o,42]
  dget "o" "a"
 -}

-- simiplified versions; working

e541b = 
  ELet "o" new $
  ELet "_" (ESet "o" "foo" 13) $
  ECall e531 [EVar "o", 42]

e541c = 
  ELet "o" new $
  ELet "_" (ESet "o" "foo" 13) $
  ELet "_" (ESet "o" "init" e531) $
  ELet "f" (EGet "o" "init") $
  ECall (EVar "f") [EVar "o",42]


e541e = 
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  -- ELet "_" (ESet "o" "class" (EVar "C")) $
  ELet "f" (EGet "C" "init") $
  ECall (EVar "f") [EVar "o",42]

-- Example 5.4.4
{-
Python example:
 class C(object):
  def m(self): 
    return 13

def m7():
  return 7

o = C()
o.m = m7
print o.m()
#END

Method call translation

o.m(args)

ifhasattr(o,m) then o.m(args) else o.class.m(o,args)


let C = new
C.m = func(self) { 13 }
let m7 = func() { 7 }
let o = new
o.class = C
o.m = m7
let f = ifhasattr(o,m) then o.m() else o.class.m(o)

-}

-- Example 5.4.4; works: "(int,[])"
e544 =  let {
    tm = tfunc [("Xs",emptyRecType)] [TVar "Xs"] TInt [] ;
    tm7 = tfunc [] [] TInt [] } in
  ELet "C" new $
  __ (ESet "C" "m" $ efunc ["self"] tm (EInt 13)) $
  ELet "m7"  (efunc [] tm7 (EInt 13)) $
  ELet "o" new $
  __ (ESet "o" "m" (EVar "m7")) $
  EIfHasAttr "o" "m" 
    (ELet "f" (EGet "o" "m") (ECall (EVar "f") []))
    (ELet "f" (EGet "C" "m") (ECall (EVar "f") [EVar "o"]))

-- Example 5.5.1; FAIL: "*** Exception: Lucretia/TypeChecker/TypeChecker.hs:197:7-49: Irrefutable pattern failed for pattern Lucretia.Types.TFunc _
e551 = efunc [] typ body where
  typ = tfunc [] [] TInt []
  body = ELabel "return" TInt $
    EBreak "return" 7

-- Auxiliary

t e = either ("ERROR:"++) show (runCheck e)
new = ENew

tfunc before params result after = 
  TFunc (M.fromList before) params result (M.fromList after)
(p,b) ==> (r,a) = tfunc b p r a

efunc args typ body = EFunc $ Func args typ (toExp body)

__ :: Exp -> Exp -> Exp
__ e = ELet "_" e

--eif :: (ToExp t, ToExp e) => Exp -> t -> e -> Exp
--eif c t e = EIf (toExp c) (toExp t) (toExp e)
eif c t e = EIf c (edo t) (edo e)

class ToExp a where
  toExp :: a -> Exp  

instance ToExp Exp where
  toExp = id

instance ToExp Bool where
  toExp True = EBoolTrue
  toExp False = EBoolFalse
  
instance ToExp Integer where
  toExp = EInt
    
instance ToExp () where
  toExp () = ENone
  
instance ToExp a => ToExp [a] where
  toExp [] = ENone
  toExp (a:as) = ELet "_" (toExp a) (toExp as)
