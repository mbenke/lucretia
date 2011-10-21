module PythonToLucretia.Converter(convertLucreciaASTToPythonAST) where

import Language.Python.Common.AST 
import Lucretia.Syntax

convertLucreciaASTToPythonAST pyAST = cModule  pyAST

notImplementedYetDef :: Def
notImplementedYetDef =
  ("_", notImplementedYetExp)

notImplementedYetExp :: Exp
notImplementedYetExp =
  ECall (EVar "print") [EInt 404]



{-
  What is below is based on 
  https://github.com/bjpop/language-python/blob/master/src/Language/Python/Common/PrettyAST.hs
-}

cModule :: Module a -> Defs
cModule (Module stmts) = map cStatement  stmts 

cIdent :: (Ident a) -> String
cIdent name = ident_string name

{-
prettyDottedName :: DottedName a -> Doc
prettyDottedName [] = empty
prettyDottedName [name] = c name
prettyDottedName (name:rest@(_:_))
   = c name <> dot <> prettyDottedName rest

cImportItem :: (ImportItem a) -> 
c (ImportItem {import_item_name = name, import_as_name = asName})
      = prettyDottedName name <+> (maybe empty (\n -> text "as" <+> c n) asName)

cFromItem :: (FromItem a) -> 
c (FromItem { from_item_name = name, from_as_name = asName })
      = c name <+> (maybe empty (\n -> text "as" <+> c n) asName) 

cFromItems :: (FromItems a) -> 
c ImportEverything {} = char '*'
c (FromItems { from_items_items = [item] }) = c item 
c (FromItems { from_items_items = items }) = parens (commaList items)

cImportRelative :: (ImportRelative a) -> 
c (ImportRelative { import_relative_dots = dots, import_relative_module = mod }) 
      = case mod of
           Nothing -> dotDoc 
           Just name -> dotDoc <> prettyDottedName name 
      where
      dotDoc = text (replicate dots '.')

prettySuite :: [Statement a] -> Doc
prettySuite stmts = vcat $ map c stmts 

optionalKeywordSuite :: String -> [Statement a] -> Doc
optionalKeywordSuite _ [] = empty
optionalKeywordSuite keyword stmts = text keyword <> colon $+$ indent (prettySuite stmts)

prettyParenList :: ConvertPythonToLucretia a => [a] -> Doc
prettyParenList = parens . commaList 

prettyOptionalList :: ConvertPythonToLucretia a => [a] -> Doc
prettyOptionalList [] = empty
prettyOptionalList list = parens $ commaList list

prettyGuards :: [(Expr a, Suite a)] -> Doc
prettyGuards [] = empty
prettyGuards ((cond,body):guards)
   = text "elif" <+> c cond <> colon $+$ indent (prettySuite body) $+$
     prettyGuards guards
-}

cStatement :: Statement a -> Def
cStatement (StmtExpr { stmt_expr = e }) = ("_", cExpr e)
cStatement _ =
  notImplementedYetDef

{-
cStatement :: (Statement a) -> 
   -- c :: Statement -> Doc 
c (Import { import_items = items}) = text "import" <+> commaList items 
c stmt@(FromImport {})
      = text "from" <+> c (from_module stmt) <+> text "import" <+> c (from_items stmt)
c stmt@(While {})
      = text "while" <+> c (while_cond stmt) <> colon $+$
        indent (prettySuite (while_body stmt)) $+$ optionalKeywordSuite "else" (while_else stmt)
c stmt@(For {})
      = text "for" <+> commaList (for_targets stmt) <+> text "in" <+> c (for_generator stmt) <> colon $+$
        indent (prettySuite (for_body stmt)) $+$ optionalKeywordSuite "else" (for_else stmt)
c stmt@(Fun {})
      = text "def" <+> c (fun_name stmt) <> parens (commaList (fun_args stmt)) <+> 
        perhaps (fun_result_annotation stmt) (text "->") <+>
     c (fun_result_annotation stmt) <> colon $+$ indent (prettySuite (fun_body stmt)) 
c stmt@(Class {})
      = text "class" <+> c (class_name stmt) <> prettyOptionalList (class_args stmt) <> 
        colon $+$ indent (prettySuite (class_body stmt)) 
c stmt@(Conditional { cond_guards = guards, cond_else = optionalElse })
      = case guards of
           (cond,body):xs -> 
              text "if" <+> c cond <> colon $+$ indent (prettySuite body) $+$ 
              prettyGuards xs $+$
              optionalKeywordSuite "else" optionalElse
   -- XXX is the assign_to always a singleton?
c (Assign { assign_to = pattern, assign_expr = e })
      = commaList pattern <+> equals <+> c e
c (AugmentedAssign { aug_assign_to = to_expr, aug_assign_op = op, aug_assign_expr = e})
      = c to_expr <+> c op <+> c e 
c (Decorated { decorated_decorators = decs, decorated_def = stmt})
      = vcat (map c decs) $+$ c stmt
c (Return { return_expr = e }) = text "return" <+> c e
c (Try { try_body = body, try_excepts = handlers, try_else = optionalElse, try_finally = finally})
      = text "try" <> colon $+$ indent (prettySuite body) $+$
        prettyHandlers handlers $+$ optionalKeywordSuite "else" optionalElse $+$ 
        optionalKeywordSuite "finally" finally 
c (Raise { raise_expr = e })
      = text "raise" <+> c e
c (With { with_context = context, with_body = body })
      = text "with" <+> hcat (punctuate comma (map prettyWithContext context)) <+> colon $+$
        indent (prettySuite body)
c Pass {} = text "pass"
c Break {} = text "break"
c Continue {} = text "continue"
c (Delete { del_exprs = es }) = text "del" <+> commaList es
c (StmtExpr { stmt_expr = e }) = c e
c (Global { global_vars = idents }) = text "global" <+> commaList idents
c (NonLocal { nonLocal_vars = idents }) = text "nonlocal" <+> commaList idents
c (Assert { assert_exprs = es }) = text "assert" <+> commaList es
c (Print { print_chevron = have_chevron, print_exprs = es, print_trailing_comma = trail_comma }) =
      text "print" <> (if have_chevron then text " >>" else empty) <+>
      hcat (punctuate comma (map c es)) <>
      if trail_comma then comma else empty
c (Exec { exec_expr = e, exec_globals_locals = gls }) = 
      text "exec" <+> c e <+> 
      maybe empty (\ (globals, next) -> text "in" <+> c globals <+>
      maybe empty (\locals -> comma <+> c locals) next) gls

prettyWithContext :: (Expr a, Maybe (Expr a)) -> Doc
prettyWithContext (e, Nothing) = c e
prettyWithContext (e, Just as) = c e <+> text "as" <+> c as

prettyHandlers :: [Handler a] -> Doc
prettyHandlers = foldr (\next rec -> c next $+$ rec) empty


cHandler :: (Handler a) -> 
c (Handler { handler_clause = exceptClause, handler_suite = suite })
      = c exceptClause <> colon $+$ indent (prettySuite suite)

cExceptClause :: (ExceptClause a) -> 
c (ExceptClause { except_clause = Nothing }) = text "except"
c (ExceptClause { except_clause = Just (e, target)}) 
      = text "except" <+> c e <+> maybe empty (\t -> text "as" <+> c t) target

cRaiseExpr :: (RaiseExpr a) -> 
c (RaiseV3 e) = 
      maybe empty (\ (x, fromE) -> c x <+> (maybe empty (\f -> text "from" <+> c f) fromE)) e
c (RaiseV2 exp) = 
      maybe empty (\ (e1, next1) -> c e1 <> 
      maybe empty (\ (e2, next2) -> comma <+> c e2 <> 
      maybe empty (\ e3 -> comma <+> c e3) next2) next1) exp

cDecorator :: (Decorator a) -> 
c (Decorator { decorator_name = name, decorator_args = args })
      = char '@' <> prettyDottedName name <+> prettyOptionalList args

cParameter :: (Parameter a) -> 
c (Param { param_name = ident, param_py_annotation = annot, param_default = def })
      = c ident <> (maybe empty (\e -> colon <> c e <> space) annot) <> 
        maybe empty (\e -> equals <> c e) def 
c (VarArgsPos { param_name = ident, param_py_annotation = annot})
      = char '*' <> c ident <> (maybe empty (\e -> colon <> c e) annot)
c (VarArgsKeyword { param_name = ident, param_py_annotation = annot })
      = text "**" <> c ident <> (maybe empty (\e -> colon <> c e) annot)
c EndPositional {} = char '*' 
c (UnPackTuple { param_unpack_tuple = tuple, param_default = def })
      = c tuple <> maybe empty (\e -> equals <> c e) def

cParamTuple :: (ParamTuple a) -> 
c (ParamTupleName { param_tuple_name = name }) = c name
c (ParamTuple { param_tuple = tuple }) = prettyParenList tuple
-}

cArgument :: (Argument a) -> Exp
cArgument (ArgExpr { arg_expr = e }) = cExpr e

{-
c (ArgVarArgsPos { arg_expr = e}) = char '*' <> c e
c (ArgVarArgsKeyword { arg_expr = e }) = text "**" <> c e
c (ArgKeyword { arg_keyword = ident, arg_expr = e }) 
      = c ident <> equals <> c e

instance ConvertPythonToLucretia t => ConvertPythonToLucretia (Comprehension t a) where
c (Comprehension { comprehension_expr = e, comprehension_for = for }) 
      = c e <+> c for 

cCompFor :: (CompFor a) -> 
c (CompFor { comp_for_exprs = es, comp_in_expr = e, comp_for_iter = iter }) 
      = text "for" <+> commaList es <+> text "in" <+> c e <+> c iter

cCompIf :: (CompIf a) -> 
c (CompIf { comp_if = e, comp_if_iter = iter }) 
      = text "if" <+> c e <+> c iter 

cCompIter :: (CompIter a) -> 
c (IterFor { comp_iter_for = compFor }) = c compFor 
c (IterIf { comp_iter_if = compIf }) = c compIf
-}

cExpr :: (Expr a) -> Exp
cExpr (Var { var_ident = i }) = EVar $ cIdent i
cExpr (Int { int_value = i }) = EInt i
cExpr (Call { call_fun = f, call_args = args }) = ECall (cExpr f) (map cArgument args)
cExpr _ = notImplementedYetExp

{-
cExpr :: (Expr a) -> 
c (Var { var_ident = i }) = c i
c (Int { expr_literal = str }) = text str 
c (LongInt { expr_literal = str }) = text str 
c (Float { expr_literal = str }) = text str 
c (Imaginary { expr_literal = str }) = text str 
c (Bool { bool_value = b}) = c b
c None {} = text "None"
c Ellipsis {} = text "..."
c (ByteStrings { byte_string_strings = bs }) = hcat (map c bs)
c (Strings { strings_strings = ss }) = hcat (map prettyString ss)
c (UnicodeStrings { unicodestrings_strings = ss }) = hcat (map prettyString ss)
c (Call { call_fun = f, call_args = args }) = c f <> prettyParenList args
c (Subscript { subscriptee = e, subscript_expr = sub })
      = c e <> brackets (c sub)
c (SlicedExpr { slicee = e, slices = ss })
      = c e <> brackets (commaList ss) 
c (CondExpr { ce_true_branch = trueBranch, ce_condition = cond, ce_false_branch = falseBranch })
      = c trueBranch <+> text "if" <+> c cond <+> text "else" <+> c falseBranch
c (BinaryOp { operator = op, left_op_arg = left, right_op_arg = right })
      = c left <> (if isDot op then dot else space <> c op <> space) <> c right
      where
      isDot (Dot {}) = True
      isDot _other = False
c (UnaryOp { operator = op, op_arg = e }) = c op <+> c e
c (Lambda { lambda_args = args, lambda_body = body })
      = text "lambda" <+> commaList args <> colon <+> c body
c (Tuple { tuple_exprs = es }) =
      case es of
         [] -> text "()"
         [e] -> c e <> comma
         _ -> commaList es
c (Yield { yield_expr = e })
      = text "yield" <+> c e
c (List { list_exprs = es }) = brackets (commaList es)
c (Dictionary { dict_mappings = mappings })
      = braces (hsep (punctuate comma $ map (\ (e1,e2) -> c e1 <> colon <> c e2) mappings))
c (Set { set_exprs = es }) = braces $ commaList es
c (ListComp { list_comprehension = lc }) = brackets $ c lc
c (Generator { gen_comprehension = gc }) = parens $ c gc
c (Paren { paren_expr = e }) = parens $ c e

cSlice :: (Slice a) -> 
c (SliceProper { slice_lower = lower, slice_upper = upper, slice_stride = stride })
      = c lower <> colon <> c upper <> (maybe empty (\s -> colon <> c s) stride)
c (SliceExpr { slice_expr = e }) = c e

cOp :: (Op a) -> 
c (And {}) = text "and"
c (Or {}) = text "or"
c (Not {}) = text "not"
c (Exponent {}) = text "**"
c (LessThan {}) = text "<"
c (GreaterThan {}) = text ">"
c (Equality {}) = text "=="
c (GreaterThanEquals {}) = text ">="
c (LessThanEquals {}) = text "<="
c (NotEquals {}) = text "!="
c (NotEqualsV2 {}) = text "<>"
c (In {}) = text "in"
c (Is {}) = text "is"
c (IsNot {}) = text "is not"
c (NotIn {}) = text "not in"
c (BinaryOr {}) = text "|"
c (Xor {}) = text "^"
c (BinaryAnd {}) = text "&"
c (ShiftLeft {}) = text "<<"
c (ShiftRight {}) = text ">>"
c (Multiply {}) = text "*"
c (Plus {}) = text "+"
c (Minus {}) = text "-"
c (Divide {}) = text "/"
c (FloorDivide {}) = text "//"
c (Invert {}) = text "~"
c (Modulo {}) = text "%"
c (Dot {}) = dot

cAssignOp :: (AssignOp a) -> 
c (PlusAssign {}) = text "+="
c (MinusAssign {}) = text "-="
c (MultAssign {}) = text "*="
c (DivAssign {}) = text "/="
c (ModAssign {}) = text "%="
c (PowAssign {}) = text "**="
c (BinAndAssign {}) = text "&="
c (BinOrAssign {}) = text "|="
c (BinXorAssign {}) = text "^="
c (LeftShiftAssign {}) = text "<<="
c (RightShiftAssign {}) = text ">>="
c (FloorDivAssign {}) = text "//="
-}
