module Lucretia.Syntax where

import Lucretia.Types
import Lucretia.Definitions (Name, Param)

type Program = Defs
type Defs = [Def]
type Def = (Name, Exp)

data Exp
    = EInt Integer
    | EBoolTrue
    | EBoolFalse
    | ENone

    | EVar Name
    | ELet Name Exp Exp
    | ELets Defs Exp
    | EIf Exp Exp Exp
    | ENew

    | EGet Name Name
    | ESet Name Name Exp
    | ELabel Name Exp --Type
    | EBreak Name Exp --Type
    | EFunc Func --Type
    | ECall Exp [Exp]

    | EAdd Exp Exp
    | EMul Exp Exp

    | EDeref Exp 

      deriving (Eq,Show)

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

