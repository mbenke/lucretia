{-# LANGUAGE FlexibleInstances #-}
module Lucretia.Parser.ApplicativeParser where
import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types
import Data.Functor
import Control.Monad
import Control.Monad.Error

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language(emptyDef)

-- Caution, Applicative.(<|>) conflicts with Parsec
import Control.Applicative(Applicative,pure,(<*>),(<*),(*>),liftA2) 



langDef = emptyDef { 
  PT.reservedNames = ["let", "in", "new", "None",
                      "if","then","else", "label", "break"]}
lexer = PT.makeTokenParser langDef

identifier = PT.identifier lexer
integer = PT.integer lexer
symbol = PT.symbol lexer
parens  = PT.parens lexer
reserved = PT.reserved lexer
kw = reserved

runParser :: String -> String -> Either ParseError Defs
runParser info input = parse pProg info input

pProg :: Parser Defs
pProg = pDefs <* eof

pDefs :: Parser Defs
pDefs = pDef `sepBy1` (symbol ";") --pOptionalSemi

pOptionalSemi :: Parser ()
pOptionalSemi = optional $ symbol ";"

pDef :: Parser Def
pDef = (try pFullDef) <|> ((\e -> ("_",e)) <$> pExp)

pFullDef = liftA2 (,)  (identifier <* symbol "=")  pExp

pExp, pTerm, pF :: Parser Exp
pExp = pFunc <|> pIf <|> pNew <|> pLet <|> pArith

pFunc = EFunDecl <$> (kw "func" *> parens pParams) <*> pExp

pParams = pParam `sepBy` (symbol ",")
pParam = identifier

pMaybeCall f = do
  pCall f <|> return f

pCall f = EFunCall f <$> parens (pExp `sepBy` (symbol ","))

-- Applicative version
pMaybeCallA :: Parser(Exp->Exp)
pMaybeCallA = pCallA <|> pure id
pCallA :: Parser(Exp->Exp)
pCallA = flip EFunCall <$> parens (pExp `sepBy` (symbol ","))

pIf = EIf <$> (kw "if" *> pExp) 
          <*> (kw "then" *> pExp) 
          <*> (kw "else" *> pExp)

pLet :: Parser Exp
pLet = ELet <$> (kw "let" *> identifier)
            <*> (symbol "=" *> pExp)
            <*> (kw "in" *> pExp)

pNew = kw "new" >> return ENew

pArith :: Parser Exp
pArith = pTerm `chainl1` pAdd

pAdd = symbol "+" >> return EAdd

pTerm = pF
pF = EInt <$> integer <|> pIdExp <|> (parens pExp >>= pMaybeCall) 
          <|> (kw "None" >> return ENone)
          
--pRec = symbol "{" >> symbol "}" >> return ERecEmpty

{- I = id I'
I' = "." id I'' | eps
I'' = "=" E | eps
-}

{-
-- Monadic version:
pIdExp = identifier >>= pIdExp'
pIdExp' :: Name -> Parser Exp
pIdExp' n = (symbol "." >> identifier >>= pIdExp'' n )
          <|> (symbol ":" >> pLabel n)
          -- <|> (return $ EVar n)
          <|> pMaybeCall (EVar n)
pIdExp'' :: Name -> Name -> Parser Exp
pIdExp'' n n2 = (symbol "=" >> (ESet n n2 <$> pExp))
                <|> (return $ EGet n n2)
-}

pIdExp :: Parser Exp               
pIdExp = (flip ($)) <$> identifier <*> pIdExp' 

pIdExp' :: Parser (Name -> Exp)
pIdExp' = pDotted 
          <|> fmap (.EVar) pMaybeCallA
          
antiEVar :: Parser (Exp->Exp) -> Parser (Name->Exp)          
antiEVar = fmap (.EVar) 

pDotted = (flip ($)) <$> (symbol "." *> identifier ) <*> pIdExp'' 

pIdExp'' :: Parser (Name -> Name -> Exp)
pIdExp'' = (\e x1 x2 -> ESet x1 x2 e) <$>(symbol "=" *>  pExp)
                <|> pure EGet 

