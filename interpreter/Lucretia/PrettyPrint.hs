{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Lucretia.PrettyPrint where

import Text.PrettyPrint
import PrettyPrintUtils

import Lucretia.Definitions
import Lucretia.Syntax
import Lucretia.Types

instance Pretty Exp where
  pretty (EInt n) = integer n
  pretty (EBoolTrue) = text "True"
  pretty (EBoolFalse) = text "False"
  pretty (ENone) = text "None"

  pretty (EVar x) = text x
  pretty (ELet x e eIn) =
    vcat
    [ text ("let "++x++" =") <+> pretty e <+> text "in"
    , pretty eIn
    ]
  --ELets
  pretty (EIf ec et ef) =
    vcat
    [ text "if" <+> pretty ec <+> text "then"
    , indent . pretty $ et
    , text "else"
    , indent . pretty $ ef
    ]
  pretty (EIfHasAttr x a et ef) =
    vcat
    [ text $ "if "++x++" hasAttr "++a++" then"
    , indent . pretty $ et
    , text "else"
    , indent . pretty $ ef
    ]
  pretty ENew = text "new"

  pretty (EGet x a) = text $ x++"."++a
  pretty (ESet x a e) = (text $ x++"."++a++" =") <+> pretty e
  pretty (ELabel l e) =
    vcat
    [ text $ l++":"
    , indent . pretty $ e
    ]
  pretty (EBreak l e) =
    vcat
    [ text $ "break "++l
    , indent . pretty $ e
    ]
  pretty (EFunc f) = pretty f
  pretty (ECall (EVar f) es) = text "call" <+> text f <> char '(' <> pretty es <> char ')'
  pretty (ECall f es)      =
    vcat
    [ text "call" <+> pretty f
    , char '(' <> pretty es <> char ')'
    ]

  pretty (EAdd e e') = op e "+" e'
  pretty (EMul e e') = op e "*" e'

  pretty _ = text "__"

op :: Exp -> String -> Exp -> Doc
op e o e' = pretty e <+> text o <+> pretty e'

instance Pretty Func where
  pretty (Func xs t eBody) =
    vcat
    [ text "func" <+> char '(' <> pretty xs <> char ')' <+> text "::" <+> pretty t
    , indent . pretty $ eBody
    ]

instance Pretty [String] where
  pretty = hcommasep . map pretty

instance Pretty [Exp] where
  pretty = hcommasep . map pretty

instance Pretty String where
  pretty = text

instance Pretty Type where
  pretty = text . show
