-----------------------------------------------------------------------------
-- |
-- Module      :  Lucretia.TypeChecker.IsomorphicModuloNames
-- Copyright   :  (c) Michał Oniszczuk 2012
--
-- Maintainer  :  mo262537@students.mimuw.edu.pl
--
-- Isomorphism between 'Type' in context of 'Constraints' which
-- diregards differences in names of variables.
-----------------------------------------------------------------------------
{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Lucretia.TypeChecker.IsomorphicModuloNames where
--module Lucretia.TypeChecker.IsomorphicModuloNames (iso) where

import Prelude hiding (any)

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable
import Data.Function (on)

import Data.List (permutations)

import Lucretia.Definitions (Var, TVar)
import Lucretia.Types

import LocalState (localState_)

-- | Minimal definition is just isoM.
class IsomorphicModuloNames a where
  -- Unwraps the result of running isoM.
  iso  :: (a, Constraints) -- ^ Is this isomorphic with…
       -> (a, Constraints) -- ^ … that.
       -> Either String () -- ^ Either: error message indicating failure or
                           -- @()@ indicating success
  iso (a, cs) (a', cs') = evalStateT (runReaderT (isoM a a') (cs, cs')) Set.empty
  isoM :: a -- ^ Is this isomorphic with…
       -> a -- ^ … that.
       -> M ()
type M = ReaderT Environment (StateT Isomorphism (Either String))
type Environment = (Constraints, Constraints)
type Isomorphism = Set (TVar, TVar) -- visited node pairs

isomorphic :: M ()
isomorphic = return ()
notIsomorphic :: String -> M ()
notIsomorphic = throwError

instance IsomorphicModuloNames Type where
  isoM (TVar n) (TVar n') = isoM n n'
  isoM (TOr s) (TOr s') = isoM' (Set.toList s) (Set.toList s')
    where
    isoM' :: [Type] -> [Type] -> M ()
    isoM' ts = isoM'' ts (permutations ts)
    isoM'' :: [Type] -> [[Type]] -> [Type] -> M ()
    isoM'' ts (ps:pss) ts' =
      (localState_ $ isoM ps ts') `catchError` \_ -> isoM'' ts pss ts'
    isoM'' ts [] ts' = throwError $ "Type mismatch:\n  " ++ show ts ++ "\n  " ++ show ts'
  isoM (TOr s) t = isoM (TOr s) (TOr $ Set.singleton t)
  isoM t (TOr s) = isoM (TOr $ Set.singleton t) (TOr s)
  isoM a a' = unless (a == a') $ notIsomorphic $ "Expected type " ++ show a' ++ " but got " ++ show a ++ ".\n"

instance IsomorphicModuloNames TVar where
  isoM n n' = do
    v <- get
    if Set.member (n, n') v
      then isomorphic
      else cont
    where
    cont = do
      v <- get
      guard $ not (memberFst n v) && not (memberSnd n' v)
      modify $ Set.insert (n, n')
      t  <- lookupM n  =<< asks fst
      t' <- lookupM n' =<< asks snd
      isoM t t'

    memberFst :: (Eq a, Foldable f) => a -> f (a, b) -> Bool
    memberFst x = any $ \(y, _) -> x == y
    memberSnd :: (Eq b, Foldable f) => b -> f (a, b) -> Bool
    memberSnd x = any $ \(_, y) -> x == y

    lookupM :: TVar -> Constraints -> M Rec
    lookupM n cs =
      case Map.lookup n cs of
	Just result -> return result
	Nothing     -> throwError $ "Cannot find type variable named: " ++ n ++ " in constraints: " ++ showConstraints cs ++ ".\n"

instance IsomorphicModuloNames (Map Var Type) where
  isoM = isoM `on` Map.toAscList
  --isoM m m' = Map.toAscList m `isoM` Map.toAscList m'

instance IsomorphicModuloNames [(Var, Type)] where
  isoM = isoM `on` map snd

instance IsomorphicModuloNames [Type] where
  isoM ts ts' =
    if length ts == length ts'
      then isoM' ts ts'
      else throwError $ "Length of " ++ show ts ++ " and " ++ show ts' ++ " is different."
    where isoM' (t:ts) (t':ts') = isoM t t' >> isoM' ts ts'
	  isoM' [] [] = isomorphic

