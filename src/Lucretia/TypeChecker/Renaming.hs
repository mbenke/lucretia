-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Renaming on Record Identifiers ('IType' @->@ 'IType').
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

--module Lucretia.TypeChecker.Renaming where
module Lucretia.TypeChecker.Renaming ( freeVariables, getRenamingOnEnv, getRenaming, applyRenaming ) where

import Prelude hiding ( any, sequence )

import Data.Map ( Map )
import Data.Map as Map
import Data.Set ( Set )
import Data.Set as Set
import Data.Foldable ( any, Foldable )
import Data.Function ( on )
import Data.Tuple ( swap )

import Control.Monad ( guard, mzero )
import Control.Monad.State ( execStateT, get, lift, modify, StateT )
import Control.Monad.Trans.Reader ( asks, ReaderT, runReaderT )
import Data.Traversable ( sequence )

import Util.Map ( findAll )

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types
import Lucretia.TypeChecker.Monad ( CM )


-- | Get all free occuring variables.
class FreeVariables a where
  freeVariables :: a -> Set IType
instance FreeVariables PrePost where
  freeVariables (PrePost pre post) = (Set.union `on` freeVariables) pre post
instance FreeVariables Constraints where
  freeVariables cs = Set.delete env (Map.keysSet cs) `Set.union` freeVariables (Map.elems cs)
instance FreeVariables a => FreeVariables [a] where
  freeVariables = Set.unions . fmap freeVariables
instance FreeVariables TOr where
  freeVariables = freeVariables . Map.elems
instance FreeVariables TSingle where
  freeVariables (TRec rec) = (freeVariables . Map.elems) rec
  freeVariables _ = Set.empty
instance FreeVariables TAttr where
  freeVariables (_, i) = Set.singleton i

-- | Renaming to actual 'IType' (at call) from expected 'IType' (at declaration).
--
-- Also serves as a list of visited node pairs.
type Renaming = Set (IType, IType)

-- | Rename using renaming
applyRenaming :: ApplyRenaming t
              => Renaming
              -> t -> t
applyRenaming renaming = ar $ functionFromSetOfPairs $ Set.map swap renaming
  where
  functionFromSetOfPairs :: Set (IType, IType) -> IType -> IType
  functionFromSetOfPairs set = fromMap $ Map.fromList . Set.toList $ set

  fromMap :: Map.Map IType IType -> IType -> IType 
  fromMap map i = Map.findWithDefault i i map

class ApplyRenaming t where
  -- | Rename the whole nested structure, starting from IType in Constraints
  -- and ending on IType in TOr. Do not rename recursively, there is no need to do it.
  ar :: (IType -> IType) -> t -> t
instance ApplyRenaming Type where
  ar f (i, pp) = (f i, ar f pp)
instance ApplyRenaming IType where
  ar f = f
instance ApplyRenaming PrePost where
  ar f (PrePost pre post) =
    PrePost
      (ar f pre)
      (ar f post)
instance ApplyRenaming Constraints where
  ar f = Map.mapKeys f . Map.map (ar f)
instance ApplyRenaming TOr where
  ar f = Map.map (ar f)
instance ApplyRenaming TSingle where
  ar f (TRec rec) = TRec $ Map.map (ar f) rec
  --ar f (TFun fun) = TFun $ ar f fun
  ar _ t = t
instance ApplyRenaming TAttr where
  ar f (definedness, i) = (definedness, f i)
instance ApplyRenaming TFunSingle where
  ar f (TFunSingle preTs (postT, ppF)) =
    TFunSingle 
      (f `fmap` preTs)
      ( (f postT)
      , (ar f ppF)
      )
 
getRenamingOnEnv :: Constraints -> Constraints -> CM Renaming
getRenamingOnEnv = getRenaming `on` \cs -> (getEnv cs, cs)

class GetRenaming a where
  -- | Gets all possible renamings.
  -- Expected condition should be w or equal to actual condition.
  getRenaming :: (a, Constraints) -- ^ @a@ at the place of call
              -> (a, Constraints) -- ^ @a@ at the place of declaration
              -> CM Renaming       -- ^ Possible renamings
  al `getRenaming` ed = lift $ run al ed
    where
    run :: GetRenaming a
        => (a, Constraints)
        -> (a, Constraints)
        -> [Renaming]
    run (a, cs) (a', cs') = execStateT (runReaderT (r a a') (cs, cs')) emptyRenaming
    emptyRenaming = Set.empty

  r :: a -- ^ Get renaming to this …
    -> a -- ^ … from that.
    -> M ()
type M = ReaderT Environment (StateT Renaming [])
type Environment = (Constraints, Constraints)

ok = return ()
getVisited :: M Renaming
getVisited = get
asksConstraints :: (Environment -> Constraints) -> M Constraints
asksConstraints = asks

instance GetRenaming [IType] where
  r (i:is) (i':is') = r i i' >> r is is'
  r []     []       = ok
instance GetRenaming IType where
  r i i' = do
    visited <- getVisited
    if alreadyFollowed (i, i') visited
      then ok
      else checkRecursively i i'
    where
      alreadyFollowed = Set.member

      checkRecursively i i' = do
        visited <- getVisited
        guard $ (i, i') `neitherMemberOf` visited
        modify $ Set.insert (i, i')
        t  <- getITypeFromConstraints i  fst
        t' <- getITypeFromConstraints i' snd
        r t t'

      getITypeFromConstraints i which = do
        cs <- asksConstraints which
        return $ Map.lookup i cs

      neitherMemberOf :: (IType, IType) -> Renaming -> Bool
      (i, i') `neitherMemberOf` visited =
        not (memberFst i  visited) &&
        not (memberSnd i' visited)
        where
        memberFst :: (Eq a, Foldable f) => a -> f (a, b) -> Bool
        memberFst x = any $ \(y, _) -> x == y
        memberSnd :: (Eq b, Foldable f) => b -> f (a, b) -> Bool
        memberSnd x = any $ \(_, y) -> x == y
instance GetRenaming (Maybe TOr) where
  r (Just t) (Just t') = r t t'
  r _ _ = ok
  -- get renaming only where possible
  -- i.e. where there is a 'TOr' defined in 'Constraints' for a corresponding 'IType'
instance GetRenaming TOr where
  r = r `on` Map.lookup KRec
instance GetRenaming (Maybe TSingle) where
  r (Just (TRec t)) (Just (TRec t')) = r t t'
  r _  _ = ok
  -- get renaming only where possible
  -- i.e. where there is are 'KRec' defined in both 'TOr' values
instance GetRenaming TRec where
  -- TODO RTR: r t t' = sequence $ intersectionWith r t t'
  r t t' = (r `on` inBoth) t t'
    where
      inBoth = fmap snd . findAll (Set.toList keysInBoth)
      keysInBoth :: Set IAttr
      keysInBoth = Map.keysSet t `Set.intersection` Map.keysSet t'
instance GetRenaming [TAttr] where
  r (i:is) (i':is') = r i i' >> r is is'
  r []     []       = ok
instance GetRenaming TAttr where
  r (_, t) (_, t') = r t t'


