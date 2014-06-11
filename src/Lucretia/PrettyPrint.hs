{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Lucretia.PrettyPrint     ( pretty ) where

import Text.PrettyPrint
import Data.List                ( intercalate )

import Util.PrettyPrint

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types

instance Pretty Exp where
  pretty (EInt n) = integer n
  pretty (EBool b) = text . show $ b
  pretty (EStr s) = text . show $ s
  pretty (ENone) = text "None"

  pretty (EVar x) = text x
  pretty (ELet x e eIn) =
    vcat
    [ text ("let "++x++" =") <+> pretty e <+> text "in"
    , pretty eIn
    ]
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
  pretty (ESet x a e) = text (x++"."++a++" = ") <+> pretty e
  pretty (EFunDecl xs eBody) =
    vcat
    [ text "func" <+> char '(' <> pretty xs <> char ')' <+> char '{'
    , indent . pretty $ eBody
    , char '}'
    ]
  pretty (EFunCall (EVar f) es) = text "call" <+> text f <> char '(' <> pretty es <> char ')'
  pretty (EFunCall f es)      =
    vcat
    [ text "call" <+> pretty f
    , char '(' <> pretty es <> char ')'
    ]

  pretty (EAdd e e') = op e "+" e'
  pretty (EMul e e') = op e "*" e'

  pretty _ = text "**"

op :: Exp -> String -> Exp -> Doc
op e o e' = pretty e <+> text o <+> pretty e'

instance Pretty [String] where
  pretty = hcommasep . map pretty

instance Pretty [Exp] where
  pretty = hcommasep . map pretty

instance Pretty String where
  pretty = text

instance Pretty Type where
  pretty = text . show
