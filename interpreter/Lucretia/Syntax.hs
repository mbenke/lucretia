module Lucretia.Syntax where

type Name = String
type Defs = [Def]
type Def = (Name,Exp)
type Param = Name

data Exp
    = EInt Integer
    | EVar Name
    | EAdd Exp Exp
    | ELet Name Exp Exp
    | ELets Defs Exp
    | EIf Exp Exp Exp
    | ENew Exp
    | EDeref Exp 
    | ENone
    | ERecEmpty
    | EGet Name Name
    | ESet Name Name Exp
    | ELabel Name Exp
    | EBreak Name Exp
    | EFunc Func
    | ECall Exp [Exp]
      deriving (Eq,Show)

data Func = Func [Param] Exp deriving (Eq,Show)

instance Num Exp where
    fromInteger = EInt
    (+) = EAdd
    (*) = undefined
    (-) = undefined
    signum = undefined
    abs = undefined
