-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Weakening rules.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances #-}
module Lucretia.TypeChecker.Weakening ( checkWeaker, weaker ) where

import Data.Map ( Map )
import Data.Map as Map
import Data.Set ( Set )
import Data.Set as Set
import Data.Function ( on )

import Prelude hiding ( sequence )
import Control.Monad ( guard )
import Data.Traversable ( sequence )

import Lucretia.Language.Definitions
import Lucretia.Language.Types
import Lucretia.TypeChecker.Monad


checkWeaker :: Constraints
            -> Constraints
            -> CM ()
checkWeaker c c' = do
  toWeaken <- weaker c c'
  guard $ toWeaken == Map.empty

weaker :: Constraints -- ^ @Constraints@ at the place of call
       -> Constraints -- ^ @Constraints@ at the place of declaration, function preconditions
       -> CM Constraints
weaker c c' = w c c'

class Weaker a where
  -- | Pump pre-constraints (looking at the post-constraints)
  -- of the whole code block (depicted as B) preceding function call,
  -- to match the called function's pre-constraints.
  -- 
  -- B ESth1
  -- B ESth2
  -- B ...
  -- B ESthN
  --   EFunCall
  w :: a    -- ^ Post-Constraints at the place of call        (actual   conditions)
    -> a    -- ^  Pre-Constraints at the place of declaration (expected conditions)
    -> CM a -- ^ What should be added to preconstraints to ensure call' <c call' + decl, where call' = m + whatShouldBeAdded

instance Weaker Constraints where
  w c c' = do
    intersected <- sequence $ Map.intersectionWith w c c'
    let intersectedNonEmpty = Map.filter nonEmpty intersected
    let onlyInDecl = c' `Map.difference` c
    return $ intersectedNonEmpty `Map.union` onlyInDecl

instance Weaker TOr where
  w m m' = do
    guard $ (Set.isSubsetOf `on` Map.keysSet) m m'
    rec <- (w `on` Map.lookup KRec) m m'
    return $ mapFromMaybe KRec rec

mapFromMaybe :: Ord k => k -> Maybe v -> Map k v
mapFromMaybe k v =
    Map.alter (\_ -> v) k Map.empty

instance Weaker (Maybe TSingle) where
  w (Just (TRec s)) (Just (TRec s')) = return $
    if nonEmpty difference
      then Just $ TRec difference
      else Nothing
    where difference = s' `Map.difference` s
  w Nothing _ = return Nothing
  -- cannot be otherwise since:
  -- * types in "m :: TOr" are subset of types in "m' :: TOr": Set.isSubsetOf `on` Map.keys $ m m'
  -- * "w" on "Maybe TSingle" parameters are TRecs: rec <- (w `on` Map.lookup KRec) m m'

nonEmpty :: Map k v -> Bool
nonEmpty = not . Map.null

