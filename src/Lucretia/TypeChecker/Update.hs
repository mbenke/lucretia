-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Update- & Extend- Constraints rules.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Lucretia.TypeChecker.Update ( update, extend ) where

import Data.Map as Map hiding ( filter, update )
import Data.Set as Set hiding ( filter, update )
import Data.Function ( on )

import Lucretia.Language.Definitions
import Lucretia.Language.Types


-- ** Type information update (Definition 3.4 in wp)
class Update a where
  update :: a -> a -> a
instance Update Constraints where
  update = Map.unionWith update
instance Update TOr where
  update = mapCombineWith update
instance Update (Maybe TSingle) where
  update (Just (TRec r)) (Just (TRec r')) = Just $ TRec (update r r')
  update _ t' = t'
instance Update TRec where
  update = Map.unionWith update
instance Update TAttr where
  update _                t'@(Required, _)  = t'
  update (definedness, i)    (Optional, i') = (definedness, merge i i')
    where merge i i' = if i == i' then i else undefinedId
  -- IType must match in both sides

-- | The only difference between update & extend is that:
-- update overrides a type in TOr, while
-- extend adds      a type in TOr
class Extend a where
  extend :: a -> a -> a
instance Extend Constraints where
  extend = Map.unionWith extend
instance Extend TOr where
  extend = Map.unionWith extend
instance Extend TSingle where
  extend (TRec r) (TRec r') = TRec $ update r r'
  extend _ t = t

mapCombineWith :: Ord k
               => (Maybe v -> Maybe v -> Maybe v)
               -> Map k v -> Map k v -> Map k v
mapCombineWith combine m m' =
  Map.fromList $
    filterJust $
      fmap (applyCombine combine m m') $
        Set.toList $ allKeys m m'
  where allKeys :: Ord k => Map k v -> Map k v -> Set k
        allKeys = Set.union `on` keysSet
        applyCombine :: Ord k
                     => (Maybe v -> Maybe v -> Maybe v)
                     -> Map k v -> Map k v
                     -> k
                     -> (k, Maybe v)
        applyCombine combine m m' k =
          ( k
          , (combine `on` Map.lookup k) m m'
          )
        filterJust :: [(k, Maybe v)] -> [(k, v)]
        filterJust xs = fmap (\(k, Just v) -> (k, v)) $ filter isJust xs
        isJust :: (k, Maybe v) -> Bool
        isJust (_, Just _ ) = True
        isJust (_, Nothing)  = False

