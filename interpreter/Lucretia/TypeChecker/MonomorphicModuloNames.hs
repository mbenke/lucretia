-----------------------------------------------------------------------------
-- |
-- Module      :  Lucretia.TypeChecker.MonomorphicModuloNames
-- Copyright   :  (c) Michał Oniszczuk 2012
--
-- Maintainer  :  mo262537@students.mimuw.edu.pl
--
-- Monomorphism between 'Type' in context of 'Constraints' which
-- diregards differences in names of variables.
-----------------------------------------------------------------------------
{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances #-}

module Lucretia.TypeChecker.MonomorphicModuloNames where
--module Lucretia.TypeChecker.MonomorphicModuloNames (weakerOrEqualTo, monoRename) where

import Prelude hiding (any)

import Control.Monad.Error (MonadError, throwError)
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable (Foldable, any, find)
import Data.Function (on)

import Data.List (permutations, subsequences)
import Data.Maybe (fromJust)
import Data.Either.Unwrap (mapLeft)

import Lucretia.Definitions (Var, TVar)
import Lucretia.Types

import MonadUtils (localState_, anyM)

-- | Monomorphism from expected (as declared in signature) to
-- actual Type Variables.
--
-- Also serves as a list of visited node pairs.
type Monomorphism = Set (TVar, TVar)

-- | Rename using monomorphism
monoRename :: Monomorphism -> Constraints -> Constraints
monoRename m = mapCs (fromPairs . Set.toList $ m)
  where
  mapCs :: (TVar -> TVar) -> Constraints -> Constraints
  mapCs f cs = Map.mapKeys f $ Map.map (mapRec f) cs
  fromPairs :: [(TVar, TVar)] -> TVar -> TVar
  fromPairs pairs = fromMap (Map.fromList pairs)
  fromMap :: Map.Map TVar TVar -> TVar -> TVar 
  fromMap map a = Map.findWithDefault a a map
  mapRec :: (TVar -> TVar) -> Rec -> Rec
  mapRec f = Map.map $ mapType f
  mapType :: (TVar -> TVar) -> Type -> Type
  mapType f (TVar n) = TVar $ f n
  mapType _ t = t
 

-- | Minimal definition is just w.
class Show a => MonomorphicModuloNames a where
  weakerOrEqualTo :: MonadError String m
                  => (a, Constraints) -- ^ Expected condition
                  -> (a, Constraints) -- ^ Actual condition
                  -> m Monomorphism -- ^ Is the expected condition weaker than the actual
  weakerOrEqualTo ed al = eitherToMonadError $ weakerOrEqualToE ed al
    where
    eitherToMonadError :: MonadError e m => Either e a -> m a
    eitherToMonadError (Left e) = throwError e
    eitherToMonadError (Right a) = return a

  -- | Unwraps the result of running w.
  weakerOrEqualToE :: (a, Constraints) -- ^ Is this monomorphic with…
                  -> (a, Constraints) -- ^ … that.
                  -> Either String Monomorphism -- ^ Either: error message indicating failure or
                           -- @()@ indicating success
  weakerOrEqualToE ed al = mapLeft (\msg -> "Expected condition "++showCondition ed++" should be weaker or equal to actual condition "++showCondition al++". They are not because: "++msg) $ run ed al
    where
    run (a, cs) (a', cs') = execStateT (runReaderT (w a a') (cs, cs')) Set.empty
    showCondition (a, cs) = "(type: "++show a++"; with constraints: "++showConstraints cs++")"

  w :: a -- ^ Is this monomorphic with…
       -> a -- ^ … that.
       -> M ()
type M = ReaderT Environment (StateT Monomorphism (Either String))
type Environment = (Constraints, Constraints)

notMonomorphic :: String -> M ()
notMonomorphic = throwError

instance MonomorphicModuloNames Type where
  w (TVar n) (TVar n') = w n n'
  w (TOr s) (TOr s') = w' (Set.toList s) (Set.toList s')
    where
    w' :: [Type] -> [Type] -> M ()
    w' ts ts' = anyM [localState_ $ w ps ps' | ps <- subsequences ts, ps' <- permutations ts'] err
      where
      err = notMonomorphic $ "type mismatch:\n  " ++ show ts ++ "\n  " ++ show ts'
  w (TOr s) t = w (TOr s) (TOr $ Set.singleton t)
  w t (TOr s) = w (TOr $ Set.singleton t) (TOr s)
  w a a' = unless (a == a') $ notMonomorphic $ "type " ++ show a' ++ " does not equal " ++ show a ++ "."

instance MonomorphicModuloNames TVar where
  w n n' = do
    v <- get
    if Set.member (n, n') v
      then return ()
      else cont
    where
    cont = do
      v <- get
      guard $ not (memberFst n v) && not (memberSnd n' v)
      modify $ Set.insert (n, n')
      t  <- lookupM n  =<< asks fst
      t' <- lookupM n' =<< asks snd
      w t t'

    memberFst :: (Eq a, Foldable f) => a -> f (a, b) -> Bool
    memberFst x = any $ \(y, _) -> x == y
    memberSnd :: (Eq b, Foldable f) => b -> f (a, b) -> Bool
    memberSnd x = any $ \(_, y) -> x == y

    lookupM :: TVar -> Constraints -> M Rec
    lookupM n cs =
      case Map.lookup n cs of
	Just result -> return result
	Nothing     -> throwError $ "cannot find type variable named: " ++ n ++ " in constraints: " ++ showConstraints cs ++ "."

instance MonomorphicModuloNames (Map Var Type) where
  w m m' =
    if Map.size m == Map.size m'
      then Map.toAscList m `w`  Map.toAscList m' -- this branch is only to produce more meaningful error messages
      else Map.toAscList m `w'` Map.toAscList m'
    where
    w' :: [(Var, Type)] -> [(Var, Type)] -> M ()
    w' ed al = [w ed s_al | s_al <- subsequences al] `anyM` (throwError $ "Expected record: "++show ed++" should be weaker (have less fields) than actual record "++show al++".")

instance MonomorphicModuloNames [(Var, Type)] where
  w = w `on` map snd

instance MonomorphicModuloNames [Type] where
  w ts ts' =
    if length ts == length ts'
      then w' ts ts'
      else throwError $ "Length of " ++ show ts ++ " and " ++ show ts' ++ " is different."
    where w' (t:ts) (t':ts') = w t t' >> w' ts ts'
	  w' [] [] = return ()

