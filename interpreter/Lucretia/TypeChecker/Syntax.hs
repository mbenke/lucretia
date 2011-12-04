module Lucretia.TypeChecker.Syntax where

type Name = String
type Program = Defs
type Defs = [Def]
type Def = (Name,Exp)
type Param = Name

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
    | ENew
--    | ENew Exp

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

data Func = Func [Param] Exp deriving (Eq,Show)

instance Num Exp where
    fromInteger = EInt
    (+) = EAdd
    (*) = undefined
    (-) = undefined
    signum = undefined
    abs = undefined

programFromExp :: Exp -> Program
programFromExp e = [("_",e)]

