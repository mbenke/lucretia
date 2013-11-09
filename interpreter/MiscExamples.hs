{-# LANGUAGE FlexibleInstances #-}

import Lucretia.Definitions
import Lucretia.TypeChecker.TypeChecker (runCheck)
import Lucretia.Syntax
import Lucretia.Types
import qualified Data.Map as M

eInt42 :: Exp
eInt42 = 42

new = ENew


t e = either ("ERROR:"++) show (runCheck e)
t1 = t eInt42
t2 = t new
elsenone = ENone
edo :: [Exp] -> Exp
edo [e] = e
edo (e:es) = ELet "_" e $ edo es
edo_ [] = ENone
edo_ (e:es) = ELet "_" e $ edo_ es
eif_ c t e = EIf c (edo_ t) (edo_ e)

-- HACK for a simpler expression syntax

class ToExp a where
  toExp :: a -> Exp  

instance ToExp Exp where
  toExp = id
data D a = D a String Exp

fromD :: D a -> Exp
fromD (D a v e) = e

instance ToExp (D a) where
  toExp = fromD
-- instance ToExp a => ToExp (D a) where  
--  toExp (D a v e) = toExp a
  
instance ToExp Bool where
  toExp = EBool

instance ToExp Integer where
  toExp = EInt
  
instance ToExp String where
  toExp = EStr
  
instance ToExp () where
  toExp () = ENone
  
instance ToExp a => ToExp [a] where
  toExp [] = ENone
  toExp (a:as) = ELet "_" (toExp a) (toExp as)
  
instance Monad D where
  return a = D a "_" ENone
  (D a v e) >>= k = D b w (ELet v e e1) where
    D b w e1 = k a
                         
dlet :: (ToExp a) => String -> a -> D Exp
dlet v e = D (EVar v) v (toExp e)

de :: (ToExp a) => a -> D Exp
de x = D e "" e where e = toExp x
dvar = de . EVar

dif :: (ToExp c,ToExp t, ToExp e) => c -> t -> e -> D Exp
dif c t e = de $ EIf (toExp c) (toExp t) (toExp e)


-- Example
-- d1 :: D Exp
d1 = do
  dlet "ha" eTrue
  dlet "m" ENew
  de $ EVar "m"
  
-- End of HACK
e521 = ELet "ha" eTrue $
       ELet "m"  ENew $
       EIf (EVar "ha")
         (ESet "m" "c" (EStr "arg"))
         (ESet "m" "c" (EStr "help"))

e522 = ELet "ha" eTrue $
       ELet "m"  ENew $
       ELet "" (eif_ (EVar "ha")
         [ESet "m" "c" (EStr "arg")]
         []) $
       EVar "m"
       
d522 :: D Exp
d522 = do
  ha <- dlet "ha" True
  m <- dlet "m" ENew
  dif ha
         [ESet "m" "c" (EStr "arg")]
         ()
  de m


efunc args body = EFunDecl args $ toExp body

e531 = efunc ["self","x"] [ESet "self" "a" (EVar "x")]


{-e543 = toExp $ do
  a <- dlet "a" ENew
  ESet "a" "equals" $ efunc ["y"] [ESet "a" "w" (EGet "y" "r")]
  de a
-}

e543 =
  ELet "eq" ( efunc ["a","b"] True) $
  ELet "a" ENew $ 
  ELet "_" (ESet "a" "w" 0) $
  ESet "a" "equals" $ 
     efunc ["y"]
       (EFunCall (EVar "eq") [EGet "a" "w",EGet "y" "r"])
{-
eget' :: Exp -> [Field] -> Exp
eget' e [] = e
eget' (EVar v) [f] = EGet v f
eget' (EVar v) (f:fs) = ELet "_" (EGet v f) (eget' (EVar "_") fs)
eget' e fs = ELet "_" e (eget' (EVar "_") fs)
eget e fs = eget' (toExp e) fs 
-}
d542 = do 
  o <- dlet "o" ENew 
  de $ ESet "o" "self" o
  s <- dlet "s" $ EGet "o" "self"
  de $ EGet "s" "self"
--  de $ eget o ["self","self"]

dnew n = dlet n new
dset v f e = de $ ESet v f (toExp e)
dget v f = de $ EGet v f

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


e541d = 
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  ELet "_" (ESet "o" "class" (EVar "C")) $
  ELet "f" (EGet "C" "init") $
  EFunCall (EVar "f") [EVar "o",42]

e541e = 
  ELet "C" new $
  ELet "_" (ESet "C" "init" e531) $
  ELet "o" new $
  -- ELet "_" (ESet "o" "class" (EVar "C")) $
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

__ :: Exp -> Exp -> Exp
__ e = ELet "_" e
e544 =  let {
  ELet "C" new $
  __ (ESet "C" "m" $ efunc ["self"] (EInt 13)) $
  ELet "m7"  (efunc [] (EInt 13)) $
  ELet "o" new $
  __ (ESet "o" "m" (EVar "m7")) $
  EIfHasAttr "o" "m" 
    (ELet "f" (EGet "o" "m") (EFunCall (EVar "f") []))
    (ELet "f" (EGet "C" "m") (EFunCall (EVar "f") [EVar "o"]))

