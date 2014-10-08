-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Map utility functions.
-----------------------------------------------------------------------------
module Util.Map ( lookupAll, findAll ) where

import Data.Map ( Map )
import Data.Map as Map


-- | Lookup values for given keys.
--
-- The function will return the corresponding values as @('Just' value)@,
-- or 'Nothing' if a key isn't in the map.
lookupAll :: Ord k
          => [k] -> Map k v
          -> [Maybe v]
lookupAll keys m = fmap (\k -> Map.lookup k m) keys

-- | Find values for given keys.
-- Calls 'error' when any of the elements can not be found.
findAll   :: Ord k
          => [k] -> Map k v
          -> [v]
findAll   keys m = fmap (m Map.!) keys

