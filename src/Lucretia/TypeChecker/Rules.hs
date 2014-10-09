-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- TypeChecker rules
-----------------------------------------------------------------------------
module Lucretia.TypeChecker.Rules ( matchBlock ) where

import Control.Monad ( guard )
import Control.Monad.State ( lift )
import Data.Map as Map hiding ( update )
import Data.Set as Set
import Data.Function ( on )

import Util.Debug

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types

import Lucretia.TypeChecker.Monad ( freshIType, CM )
import Lucretia.TypeChecker.Renaming ( applyRenaming, getRenamingOnEnv, freeVariables )
import Lucretia.TypeChecker.Update ( update, extend )
import Lucretia.TypeChecker.Weakening ( checkWeaker, weaker )


matchBlock :: Defs -> PrePost -> CM Type
matchBlock xs pp = matchBlockT xs (undefinedId, pp)

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
  TFunSingle tsDecl iDecl ppDecl <- getFunType f (_post ppCall)
  checkArgsLength xsCall tsDecl
  let ppInherited = ppDecl -- TODO InheritedPP

  let ppWithArgs = addArgsToEnv xsCall tsDecl ppInherited
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
      guard $ length xsCall == length tsDecl
        --("Function "++f++" is applied to "++show (length xsCall)++" arguments, but "++show (length tsDecl)++" arguments should be provided.\n")

    addArgsToEnv :: [IAttr] -> [IType] -> PrePost -> PrePost
    addArgsToEnv xsCall tsDecl ppInherited =
      let args = zip xsCall tsDecl in
      let newEnv = recFromPairs args in
      setEnv newEnv ppInherited

    --TODO impl
    --setEnv :: 
    setEnv = undefined
    --ppFromType `modL` (\pp -> pp `update` (env, newEnv)) $ tDeclInherited
    recFromPairs :: [(IAttr, IType)] -> TOr
    recFromPairs = undefined
    --recFromPairs xs = f


-- TODO change to [TFunSingle]
matchExp (EFunDef argNames maybeSignature funBody) _ = do
  funType <- case maybeSignature of
    Just signature -> checkSignature signature
    Nothing        -> inferSignature argNames funBody
  postPointer $ tOrFromTFunSingle funType

  where
    checkSignature :: TFunSingle -> CM TFunSingle
    checkSignature declared@(TFunSingle argTypes iDecl ppDecl) = do
      let argsPP = argsPPfromTypes argNames argTypes
          -- add pre_ ppDecl (function signatures) as post
      infered <- matchBody funBody argTypes argsPP
      --guard $ iDecl == iInfered
      checkPreWeaker  declared infered
      checkPostWeaker infered declared
      -- TODO clean constraints
      return declared
        where
        checkPreWeaker  :: TFunSingle -> TFunSingle -> CM ()
        checkPreWeaker  = checkWeaker `on` _pre  . funPP
        checkPostWeaker :: TFunSingle -> TFunSingle -> CM ()
        checkPostWeaker = checkWeaker `on` _post . funPP

    inferSignature :: [IVar] -> Defs -> CM TFunSingle
    inferSignature argNames funBody = do
      let argTypes = fmap (\n -> "A"++n) argNames
      let argsPP = argsPPfromTypes argNames argTypes
      matchBody funBody argTypes argsPP

    matchBody :: Defs -> [IType] -> PrePost -> CM TFunSingle
    matchBody funBody argTypes argsPP = do
      (funReturnId, funBodyPP) <- matchBlock funBody argsPP
      checkEmptyPreEnv funBodyPP
      let funBodyNoEnvPP = eraseEnv funBodyPP
      -- TODO clean constraints
      return $ TFunSingle argTypes funReturnId funBodyNoEnvPP

        where
        -- | Checks that no variable was referenced, apart from the arguments
        checkEmptyPreEnv :: PrePost -> CM ()
        checkEmptyPreEnv pp = guard $ getEnv (_pre pp) == emptyRec

        eraseEnv :: PrePost -> PrePost
        eraseEnv (PrePost pre post) = PrePost (eraseEnv' pre) (eraseEnv' post)
        eraseEnv' :: Constraints -> Constraints
        eraseEnv' = Map.delete env


    argsPPfromTypes argNames argTypes =
      let argsEnvRec = Map.fromList $ zip argNames (requiredList argTypes) in 
          postFromTRec env argsEnvRec

    postFromTRec :: IType -> TRec -> PrePost
    postFromTRec env argsEnvRec =
      PrePost emptyConstraints
              (singletonConstraint env $ tOrFromTRec argsEnvRec)

postPointerPrimitive :: Kind -> CM Type
postPointerPrimitive kind = postPointer $ tOrPrimitive kind

postPointer :: TOr -> CM Type
postPointer tOr = return (xId, PrePost emptyConstraints (Map.insert xId tOr emptyConstraints))

required :: IType -> TAttr
required i = (Required, i)

requiredList :: [IType] -> [TAttr]
requiredList = fmap required


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

