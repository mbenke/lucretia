-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- TypeChecker rules
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts #-}

module Lucretia.TypeChecker.Rules ( matchBlock ) where

import Prelude hiding ( error )
import Control.Monad.State ( lift )
import Data.Map as Map hiding ( update )
import Data.Set as Set
import Data.Function ( on )

import Util.OrFail ( orFail )

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types

import Lucretia.TypeChecker.Monad ( error, freshIType, CM )
import Lucretia.TypeChecker.Monad ( freshIType, CM )
import Lucretia.TypeChecker.Renaming ( applyRenaming, getRenaming, getRenamingOnEnv, freeVariables )
import Lucretia.TypeChecker.Update ( update, extend )
import Lucretia.TypeChecker.Weakening ( checkWeaker, weaker )


matchBlock :: Defs -> Constraints -> CM Type
matchBlock xs post = matchBlockT xs (undefinedId, PrePost emptyConstraints post)

-- | Bind Pre- & Post-Constraints 'pp' of a code block B with Type of the next statement x, producing Type of the whole Defs including statement x.
-- B x1
-- B x2
-- B ...
-- B xN
--   x
matchBlockT :: Defs -> Type -> CM Type
matchBlockT [] t = return t
matchBlockT (x:xs) (_, pp) = do
  xT     <- matchDefFresh x pp
  boundT <- bind pp xT
  matchBlockT xs boundT

bind :: PrePost -> Type -> CM Type
bind ppCall (iDecl, ppDecl) = do
  renaming      <- _post ppCall `getRenamingOnEnv` _pre ppDecl
  let ppRenamed  = applyRenaming renaming ppDecl

  toWeaken      <- _post ppCall `weaker` _pre ppRenamed
  let preMerged  = _pre  ppCall `extend` toWeaken
  -- Q why not:    _post ppCall `extend` toWeaken
  -- A toWeaken is already inside _post ppRenamed
  -- because: toWeaken `isSubsetOf` _pre ppRenamed && _pre ppRenamed `isSubsetOf` _post ppRenamed
  let postMerged = _post ppCall `update` _post ppRenamed
  
  return ( applyRenaming renaming iDecl
         , PrePost preMerged postMerged
         )

-- All @IType@ variables in the returned @Type@ must be fresh, so there is no risk of @IType@ name clashes when @'bind'ing@ @Type@ of the current @Def@ to the @PrePost@ of the block preceding the @Def@.
matchDefFresh :: Def -> PrePost -> CM Type

matchDefFresh (Return e) pp = matchExpFresh e pp

matchDefFresh (SetVar x e) pp = do
  (eId, ePP) <- matchExpFresh e pp
  let setPP = setVar x eId
  bind ePP (eId, setPP)

  --HERE why binding & renaming works? G make sure it works
  -- (eId, ePP) <- matchAndRename
  -- let setPP = setVar x eId
  -- bind (Set.singleton eId) ePP (eId, setPP)

  where setVar x eId = PrePost pre post
          where pre  = Map.fromList [ toEmptyRec     env       ]
                post = Map.fromList [ toSingletonRec env x eId ]

matchDefFresh (SetAttr x a e) pp = do
  (eId, ePP) <- matchExpFresh e pp
  xId <- freshIType
  let setPP = setAttr x xId a eId
  bind ePP (eId, setPP)

  where setAttr x xId a eId = PrePost pre post
          where pre  = Map.fromList [ toSingletonRec env x xId
                                    , toEmptyRec     xId
                                    ]
                post = Map.fromList [ toSingletonRec env x xId
                                    , toSingletonRec xId a eId
                                    ]

-- | 'match' rules to an @Exp@ producing Type, then rename all @ITypes@ to fresh variables in that type.
matchExpFresh :: Exp  -> PrePost -> CM Type
matchExpFresh e pp = do
  t <- matchExp e pp
  renameToFresh t

    where
    renameToFresh :: Type -> CM Type
    renameToFresh t@(_, pp) = do
      let usedITypes = Set.toList $ freeVariables pp
      renaming <- mapM usedToFresh usedITypes
      return $ applyRenaming (Set.fromList renaming) t

    usedToFresh :: IType -> CM (IType, IType)
    usedToFresh iUsed = do iFresh <- freshIType
                           return (iFresh, iUsed)
  
-- | Get Type (Pre- & Post-Constraints) of the next expression e. Type of a preceding block of statements B is given, so that the rule for a function call can read a called function signature.
-- B e1
-- B e2
-- B ...
-- B eN
--   e
matchExp  :: Exp  -> PrePost -> CM Type

matchExp (EGetVar a)    _ = return (aId, PrePost constraints constraints)
  where constraints = Map.fromList [ toSingletonRec env a aId ]

matchExp (EGetAttr x a) _ = return (aId, PrePost constraints constraints)
  where constraints = Map.fromList [ toSingletonRec env x xId
                                   , toSingletonRec xId a aId
                                   ]

matchExp (EInt _)    _ = postPointerPrimitive KInt
matchExp (EString _) _ = postPointerPrimitive KString
matchExp (EBool _)   _ = postPointerPrimitive KBool
matchExp  ENone      _ = postPointerPrimitive KNone
matchExp  ENew       _ = postPointer tOrEmptyRec

matchExp (EFunCall f xsCall) ppCall = do
  -- Pre- & post- constraints must be declared in the code.
  -- In case of other function signatures, InheritedPP should be
  -- replaced in matchExp (EFunDef ...).
  TFunSingle tsDecl iDecl (DeclaredPP ppDecl) <- getFunType f (_post ppCall)
  checkArgsLength xsCall tsDecl
  let ppInherited = inheritPP ppDecl
      -- Q how it should work

  let ppWithArgs = addArgsPP xsCall tsDecl ppInherited
  return (iDecl, ppWithArgs)

    where
    getFunType :: IVar -> Constraints -> CM TFunSingle
    getFunType f cs =
      case f `lookupInEnv` cs of
        Nothing               -> error $ "Function "++f++" is undefined."
        Just (Optional, _)    -> error $ "Function "++f++" may be undefined."
        Just (Required, ifun) -> case ifun `lookupInConstraints` cs of
           Just tfun -> unwrapFunFromOr tfun
           Nothing   -> error $ "Please declare signature for the function "++f++". Infering type of a function passed as a parameter to another function (higher order function type inference) is not supported yet."

    unwrapFunFromOr :: TOr -> CM TFunSingle
    unwrapFunFromOr tOr =
      case Map.toList tOr of
        [(KFun, TFun tfun)] -> return tfun
        otherwise           -> error $ "Variable "++f++" should be a function."

    -- | Check length of arguments,
    -- should be the same in the declaration and the place of call
    checkArgsLength xsCall tsDecl =
      (length xsCall == length tsDecl) `orFail`
        ("Function "++f++" is applied to "++show (length xsCall)++" argument(s), but "++show (length tsDecl)++" argument(s) should be provided.\n")


-- TODO implement TFunOr
matchExp (EFunDef argNames maybeSignature funBody) _ = do
  funType <- case maybeSignature of
    Just signature -> checkSignature signature
    Nothing        -> inferSignature argNames funBody
  postPointer $ tOrFromTFunSingle funType

  where
    checkSignature :: TFunSingle -> CM TFunSingle
    checkSignature decl@(TFunSingle argTypes iDecl (DeclaredPP ppDecl)) = do
      -- pre- & post- constraints must be declared in the code
      -- that should be checked by Lucretia parser
      -- case maybePPDecl of
      --   InheritedPP -> error $ "Containig function must declare pre- and post-constraints."
      --   DeclaredPP ppDecl ->
      --     do
      --
      -- We are adding pre-constraints from the function signature
      -- to make available at the call site the signatures
      -- of the functions passed as parameters
      let ppInherited = inheritPP ppDecl
      let argCs = addArgsCs argNames argTypes (_pre ppInherited)
      TFunSingle _ iInfered (DeclaredPP ppInfered) <- matchBody funBody argTypes argCs
      _pre ppInherited `checkWeaker` _pre ppInfered
      checkPost argNames iInfered (_post ppInfered) iDecl (_post ppInherited)
      -- TODO clean constraints
      return decl

    checkPost :: [IType] -> IType -> Constraints -> IType -> Constraints -> CM ()
    checkPost argNames iInfered csInfered iDecl csDecl = do
      renaming <- (iDecl:argNames, csDecl) `getRenaming` (iInfered:argNames, csInfered)
      (iDecl == applyRenaming renaming iInfered) `orFail`
        ("Returned type of a function was declared "++iDecl++" but it is "++iInfered)
      applyRenaming renaming csInfered `checkWeaker` csDecl
            

    inferSignature :: [IVar] -> Defs -> CM TFunSingle
    inferSignature argNames funBody = do
      let argTypes = fmap (\n -> "A"++n) argNames
      let argCs = addArgsCs argNames argTypes emptyConstraints
      matchBody funBody argTypes argCs

    matchBody :: Defs -> [IType] -> Constraints -> CM TFunSingle
    matchBody funBody argTypes argCs = do
      (funReturnId, funBodyPP) <- matchBlock funBody argCs
      checkEmptyPreEnv funBodyPP
      let funBodyNoEnvPP = eraseEnv funBodyPP
      -- TODO clean constraints
      return $ TFunSingle argTypes funReturnId (DeclaredPP funBodyNoEnvPP)

        where
        -- | Checks that no variable was referenced, apart from the arguments
        checkEmptyPreEnv :: PrePost -> CM ()
        checkEmptyPreEnv pp = (getEnv (_pre pp) == emptyRec) `orFail` "Inside a function body a variable was referenced which was not defined and is not in the function's parameters."

        eraseEnv :: PrePost -> PrePost
        eraseEnv (PrePost pre post) = PrePost (eraseEnv' pre) (eraseEnv' post)
        eraseEnv' :: Constraints -> Constraints
        eraseEnv' = Map.delete env

addArgsPP :: [IAttr] -> [IType] -> PrePost -> PrePost
addArgsPP argNames argTypes (PrePost pre post) =
  PrePost
    (addArgsCs argNames argTypes pre)
    (addArgsCs argNames argTypes post)

addArgsCs :: [IVar] -> [IType] -> Constraints -> Constraints
addArgsCs argNames argTypes = Map.insert env (argsTOr argNames argTypes)

argsTOr :: [IVar] -> [IType] -> TOr
argsTOr argNames argTypes = tOrFromTRec $ Map.fromList $ zip argNames (requiredList argTypes)


postPointerPrimitive :: Kind -> CM Type
postPointerPrimitive kind = postPointer $ tOrPrimitive kind

postPointer :: TOr -> CM Type
postPointer tOr = return (xId, PrePost emptyConstraints (Map.insert xId tOr emptyConstraints))

required :: IType -> TAttr
required i = (Required, i)

requiredList :: [IType] -> [TAttr]
requiredList = fmap required

inheritPP :: PrePost -> PrePost
inheritPP pp = inherit pp pp
class InheritPP a where
  inherit :: PrePost -> a -> a
instance InheritPP PrePost where
  inherit pp (PrePost pre post) = PrePost (inherit pp pre) (inherit pp post)
instance InheritPP Constraints where
  inherit pp = Map.map $ inherit pp
instance InheritPP TOr where
  inherit pp = Map.map $ inherit pp
instance InheritPP TSingle where
  inherit pp (TFun tfun) = TFun $ inherit pp tfun
  inherit pp other = other
instance InheritPP TFunSingle where
  inherit pp (TFunSingle funArgs funRet funPP) = TFunSingle funArgs funRet $ inherit pp funPP
instance InheritPP FunPrePost where
  inherit pp InheritedPP = DeclaredPP pp
  inherit pp other = other


-- 
-- matchExp _ (EIf x e1 e2) = do
--   (t1, pp1) <- matchExp e1 env
--   (t2, pp2) <- matchExp e2 env
--   let branchesMerged = ( mergeT  t1  t2
--                        , mergePP pp1 pp2 )
--   bind (isBool x) branchesMerged
-- 
-- isBool :: IVar -> CM Type
-- isBool x = return ("dummy", PrePost constraints constraints)
--   where constraints :: Constraints
--         constraints = Map.fromList [ envToX x
--                                    , xToBool
--                                    ]
--         envToX  = (env, tOrSingletonRec x (Required, xId))
--         xToBool = (xId, tOrPrimitive KBool)
--
--
-- merge ::
-- merge = do
--   Rename pp2 to pp1
--   merge
--   guard $ t1 = rename t2
--   return (t1, mergedPP)
--
