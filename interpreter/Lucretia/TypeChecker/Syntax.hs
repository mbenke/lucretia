module Lucretia.TypeChecker.Syntax where

import Lucretia.TypeChecker.Types
import Lucretia.TypeChecker.Definitions(Name, Param)

type Program = Defs
type Defs = [Def]
type Def = (Name,Exp)

-- now this file is almost the same as Lucretia.Syntax
-- TODO eliminate differences
--
data Exp 
    = EInt Integer
    | EBoolTrue
    | EBoolFalse
    | ENone

    | EVar Name
    | ELet Name Exp Exp
    | ELets Defs Exp
    | EIf Exp Exp Exp
--    | ENew Exp
    | ENew

    | EGet Name Name
    | ESet Name Name Exp
    | ELabel Name Exp --Type
    | EBreak Name Exp --Type
    | EFunc Func --Type
    | ECall Exp [Exp]

    | EAdd Exp Exp

--    | EDeref Exp 
--    | ERecEmpty

      deriving (Eq,Show)

--data Func = Func [Param] Exp deriving (Eq,Show)
data Func = Func [Param] Type Exp deriving (Eq,Show)

instance Num Exp where
    fromInteger = EInt
    (+) = EAdd
    (*) = undefined
    (-) = undefined
    signum = undefined
    abs = undefined

programFromExp :: Exp -> Program
programFromExp e = [("_",e)]

