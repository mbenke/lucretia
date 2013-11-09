module Lucretia.Syntax where

import Lucretia.Types
import Lucretia.Definitions (Var, Field, Label, Param)

type Program = Defs
type Defs = [Def]
type Def = (Var, Exp)

data Exp 
    = EInt Integer
    | EBool Bool
    | EStr String
    | ENone -- ^ () in wp
    | ENew

    | EAdd Exp Exp
    | EMul Exp Exp

    | EVar Var
    | ELet Var Exp Exp

    | EGet Var Field
    | ESet Var Field Exp

    | EIf Exp Exp Exp
    | EIfHasAttr Var Field Exp Exp

    | EFunDecl [Var] Exp
    | EFunCall Exp [Exp]

      deriving (Eq,Show)

programFromExp :: Exp -> Program
programFromExp e = [("_",e)]


