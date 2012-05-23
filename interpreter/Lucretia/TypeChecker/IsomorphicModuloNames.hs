{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Lucretia.TypeChecker.IsomorphicModuloNames (iso) where

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

import Lucretia.TypeChecker.Definitions (Name, Param)
import Lucretia.TypeChecker.Types

class IsomorphicModuloNames a where
  iso  :: (a, Constraints) -> (a, Constraints) -> Either String ()
  iso (a, cs) (a', cs') = evalStateT (runReaderT (isoM a a') (cs, cs')) Set.empty
  isoM :: a -> a -> M ()

type M = ReaderT (Constraints, Constraints) (StateT Isomorphism (Either String))
type Isomorphism = Set (Name, Name) -- visited node pairs

isomorphic :: M ()
isomorphic = return ()
notIsomorphic :: String -> M ()
notIsomorphic = throwError

instance IsomorphicModuloNames Type where
  isoM (TVar n) (TVar n') = isoM n n'
  isoM a a' = unless (a == a') $ notIsomorphic $ "Expected type " ++ show a' ++ " but got " ++ show a ++ ".\n"

instance IsomorphicModuloNames Name where
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

    lookupM :: Name -> Constraints -> M RecType
    lookupM n cs =
      case Map.lookup n cs of
	Just result -> return result
	Nothing     -> throwError $ "Cannot find type variable named: " ++ n ++ " in constraints: " ++ showConstraints cs ++ ".\n"

instance IsomorphicModuloNames (Map Name Type) where
  isoM = isoM `on` Map.toAscList
  --isoM m m' = Map.toAscList m `isoM` Map.toAscList m'

instance IsomorphicModuloNames [(Name, Type)] where
  isoM ((_, t):xs) ((_, t'):xs') = isoM t t' >> isoM xs xs'
  isoM [] [] = isomorphic

