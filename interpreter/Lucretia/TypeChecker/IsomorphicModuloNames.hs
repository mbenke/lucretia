{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Lucretia.TypeChecker.IsomorphicModuloNames (iso) where

import Prelude hiding (any)

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable

import Lucretia.TypeChecker.Definitions (Name, Param)
import Lucretia.TypeChecker.Types

class IsomorphicModuloNames a where
  isoM :: (a, Constraints) -> (a, Constraints) -> M ()
  iso  :: (a, Constraints) -> (a, Constraints) -> Either String ()
  iso a1 a2 = evalStateT (isoM a1 a2) Set.empty

type M = StateT Isomorphism (Either String)
type Isomorphism = Set (Name, Name) -- visited node pairs

isomorphic :: M ()
isomorphic = return ()
notIsomorphic :: String -> M ()
notIsomorphic why = throwError why

instance IsomorphicModuloNames Type where
  isoM (TVar n, cs) (TVar n', cs') =
    isoM (n, cs) (n', cs')
  isoM (a, _) (a', _) =
    unless (a == a') $ notIsomorphic $ "Expected type " ++ show a' ++ " but got " ++ show a ++ ".\n"

instance IsomorphicModuloNames Name where
  isoM (n, cs) (n', cs') = do
    {- if we would like to throw the error the other way
    -}
    -- TODO byc moze tu da sie sprawdzac czy (n, n') bylo odwiedzone
    --
    -- TODO:
    -- if (S.contains (n, n') < get) then ok
				     -- else guard not contains (n, _) or (_, n')
    --guard $ 
    --TODO (||)
    --  <$> (withState $ S.member (n, n'))
    --  <*> ((&&) <$> (not $ memberFst n) <*> (not $ memberSnd n') <*> isoM (lookupOrFail n cs, cs)
    v <- get
    if Set.member (n, n') v
      then isomorphic
      else a
    where
    a = do
      v <- get
      guard $ not (memberFst n v) && not (memberSnd n' v)
      modify $ Set.insert (n, n')
      t  <- lookupOrFail n  cs
      t' <- lookupOrFail n' cs'
      isoM (t, cs) (t', cs')

    memberFst :: (Eq a, Foldable f) => a -> f (a, b) -> Bool
    memberFst x = any $ \(y, _) -> x == y
    memberSnd :: (Eq b, Foldable f) => b -> f (a, b) -> Bool
    memberSnd x = any $ \(_, y) -> x == y

    lookupOrFail :: Name -> Constraints -> M RecType
    lookupOrFail n cs =
      case Map.lookup n cs of
	Just result -> return result
	Nothing     -> throwError $ "Cannot find type variable named: " ++ n ++ " in constraints: " ++ showConstraints cs ++ ".\n"

instance IsomorphicModuloNames (Map Name Type) where
  (m, cs) `isoM` (m', cs') = do
    guard $ Map.size m == Map.size m'
    (Map.toAscList m, cs) `isoM` (Map.toAscList m', cs')

--TODO extract Constraints to monad state
instance IsomorphicModuloNames [(Name, Type)] where
  ((_, t):xs, cs) `isoM` ((_, t'):xs', cs') = do
    (t,  cs) `isoM` (t',  cs')
    (xs, cs) `isoM` (xs', cs')
  ([], _)         `isoM` ([], _)            = isomorphic

