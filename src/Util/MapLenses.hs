module Util.MapLenses (mapInsertLens) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Lens (Lens, lens)
import Data.Lens.Template (makeLens)
import Prelude hiding ((.))
import Control.Category ((.))

-- | A lens that:
--
-- * for setter: does not allow to delete @k@ from the map
--
-- * for getter: assumes that @k@ is present in the map
--
mapInsertLens :: Ord k => k -> Lens (Map k v) v
mapInsertLens k = lens (\m -> m Map.! k) (Map.insert k)

