{-# LANGUAGE FlexibleInstances #-}

import Lucretia.Definitions
import Lucretia.TypeChecker.TypeChecker (runCheck)
import Lucretia.Syntax
import Lucretia.Types
import qualified Data.Map as M

-- Use this function for interactive testing, e.g.
-- test e521
test :: Exp -> String
test e = either ("ERROR:"++) show (runCheck e)

-- Example 5.2.1; works: "(X1,[X1 < {c:string}])"
e521 = ELet "ha" eTrue $
       ELet "m"  ENew $
       __ (eif (EVar "ha")
         [ESet "m" "c" (EStr "arg")]
         [ESet "m" "c" (EStr "help")]) $
       EVar "m" 
       
-- Example 5.2.2; works: "(X1,[X1 < {c:string v undefined}])"
e522 = ELet "ha" eTrue $
       ELet "m"  ENew $
       __ (eif (EVar "ha")
         [ESet "m" "c" (EStr "arg")]
         []) $
       EVar "m"

-- Example 5.3.1; works: "([Xs < {}] Xs int -> NoneType [Xs < {a:int}],[])"
e531 = efunc ["self","x"] [ESet "self" "a" (EVar "x")]

e531b = EFunCall f [new, 42] where
  f = efunc ["self","x"] (__ (ESet "self" "a" (EVar "x")) $ EVar "self")
-- Example 5.4.2; works: "(X1,[X1 < {self:X1}])"
e542 = 
  ELet "o" new $
  __ (ESet "o" "self" (EVar"o")) $
  ELet "s" (EGet "o" "self") $
  EGet "s" "self"

-- Example 5.4.3; works: (X1,[X1 < {equals:[Xa < {w:int},Y < {r:int}];a:Xa, eq:[];;int int -> bool [];Y -> bool [], w:int}])
e543 =
  ELet "eq" (efunc ["a","b"] True) $
  ELet "a" ENew $ 
  ELet "_" (ESet "a" "w" 0) $
  ESet "a" "equals" $ 
     efunc ["y"] $
       EFunCall (EVar "eq") [EGet "a" "w",EGet "y" "r"])


-- Example 5.4.1; simplified; works
e541d = 
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  ELet "_" (ESet "o" "class" (EVar "C")) $
  ELet "f" (EGet "C" "init") $
  __ (EFunCall (EVar "f") [EVar "o",42]) $
  EGet "o" "a"
  
-- here is what we want in the end; should be int
e541 = 
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  ELet "_" (ESet "o" "class" (EVar "C")) $
  ELet "c" (EGet "o" "class") $
  ELet "f" (EGet "c" "init") $
  __ (EFunCall (EVar "f") [EVar "o",42]) $
  -- EGet "o" "class"
  EVar "o"
{-
d541a = do
  dlet "m" new
  -- de $ ESet "m" "C" new
  c <- dlet "C" new
  
  let init = efunc ["self","x"] [ESet "self" "a" (EVar "x")]
  dset "C" "init" init
  o <- dlet "o" new
  dset "o" "class" c
  f <- dlet "f" (EGet "C" "init")
  de $ EFunCall f [o,42]
  dget "o" "a"
 -}

-- simiplified versions; working

e541b = 
  ELet "o" new $
  ELet "_" (ESet "o" "foo" 13) $
  EFunCall e531 [EVar "o", 42]

e541c = 
  ELet "o" new $
  ELet "_" (ESet "o" "foo" 13) $
  ELet "_" (ESet "o" "init" e531) $
  ELet "f" (EGet "o" "init") $
  EFunCall (EVar "f") [EVar "o",42]


e541e = 
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  ELet "_" (ESet "o" "class" (EVar "C")) $
  ELet "f" (EGet "C" "init") $
  EFunCall (EVar "f") [EVar "o",42]

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
  ELet "C" new $
  __ (ESet "C" "m" $ efunc ["self"] (EInt 13)) $
  ELet "m7"  (efunc [] (EInt 13)) $
  ELet "o" new $
  __ (ESet "o" "m" (EVar "m7")) $
  EIfHasAttr "o" "m" 
    (ELet "f" (EGet "o" "m") (EFunCall (EVar "f") []))
    (ELet "f" (EGet "C" "m") (EFunCall (EVar "f") [EVar "o"]))

-- Example 5.4.5;
e545 = undefined

e545b = efunc ["n"] $ ELet "f" (EGet "m" "f") b
        where b = EFunCall (EVar "f") [EVar "n"]

-- Auxiliary

new = ENew


efunc args body = EFunDecl args $ toExp body

__ :: Exp -> Exp -> Exp
__ e = ELet "_" e

--eif :: (ToExp t, ToExp e) => Exp -> t -> e -> Exp
--eif c t e = EIf (toExp c) (toExp t) (toExp e)
eif c t e = EIf c (edo t) (edo e)

edo :: [Exp] -> Exp
edo [] = ENone
edo (e:es) = __ e (edo es)

class ToExp a where
  toExp :: a -> Exp  

instance ToExp Exp where
  toExp = id

instance ToExp Bool where
  toExp = EBool

instance ToExp Integer where
  toExp = EInt
    
instance ToExp () where
  toExp () = ENone
  
instance ToExp a => ToExp [a] where
  toExp [] = ENone
  toExp (a:as) = ELet "_" (toExp a) (toExp as)
