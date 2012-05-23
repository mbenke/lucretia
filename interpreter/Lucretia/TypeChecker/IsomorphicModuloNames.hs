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
import Data.Function (on)

import Lucretia.TypeChecker.Definitions (Name, Param)
import Lucretia.TypeChecker.Types

class IsomorphicModuloNames a where
  iso  :: (a, Constraints) -> (a, Constraints) -> Either String ()
  iso (a, cs) (a', cs') = evalStateT (isoM cs cs' a a') Set.empty
  isoM :: Constraints -> Constraints -> a -> a -> M ()

type M = StateT Isomorphism (Either String)
type Isomorphism = Set (Name, Name) -- visited node pairs

isomorphic :: M ()
isomorphic = return ()
notIsomorphic :: String -> M ()
notIsomorphic why = throwError why

instance IsomorphicModuloNames Type where
  isoM cs cs' (TVar n) (TVar n') = isoM cs cs' n n'
  isoM cs cs' a a' = unless (a == a') $ notIsomorphic $ "Expected type " ++ show a' ++ " but got " ++ show a ++ ".\n"

instance IsomorphicModuloNames Name where
  isoM cs cs' n n' = do
    v <- get
    if Set.member (n, n') v
      then isomorphic
      else cont
    where
    cont = do
      v <- get
      guard $ not (memberFst n v) && not (memberSnd n' v)
      modify $ Set.insert (n, n')
      -- TODO OPT RTR using lenses
      -- http://stackoverflow.com/questions/8469044/template-haskell-with-record-field-name-as-variable
      t  <- lookupM n  cs
      t' <- lookupM n' cs'
      isoM cs cs' t t'

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
  isoM cs cs' = (isoM cs cs') `on` Map.toAscList
  --isoM m m' = Map.toAscList m `isoM` Map.toAscList m'

instance IsomorphicModuloNames [(Name, Type)] where
  isoM cs cs' ((_, t):xs) ((_, t'):xs') = isoM cs cs' t t' >> isoM cs cs' xs xs'
  isoM cs cs' [] [] = isomorphic

