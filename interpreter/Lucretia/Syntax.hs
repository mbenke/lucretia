module Lucretia.Syntax where

import Lucretia.Types
import Lucretia.Definitions (Var, Field, Label, Param)

type Program = Defs
type Defs = [Def]
type Def = (Var, Exp)

data Exp 
    = EInt Integer
    | EBoolTrue
    | EBoolFalse
    | ENone

    | EVar Var
    | ELet Var Exp Exp
    | ELets Defs Exp
    | EIf Exp Exp Exp
    | EIfHasAttr Var Field Exp Exp
    | ENew

    | EGet Var Field
    | ESet Var Field Exp
    | ELabel Label Exp
    | EBreak Label Exp
    | EFunc Func
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


