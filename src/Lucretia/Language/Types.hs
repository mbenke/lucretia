{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts #-}

module Lucretia.Language.Types where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lucretia.Language.Definitions (Var, TVar, Field)

import Data.Lens (Lens, mapLens, getL)
import Data.Lens.Template (makeLens)
import Prelude hiding ((.))
import Control.Category ((.))
import Util.MapLenses (mapInsertLens)

-- * Language.Types (/Defition 2.1 (Language.Types)/ in wp)

-- | @t_b@ /and/ @t@ in wp
data Type
  = TInt -- ^ @c@ in wp
  | TBool -- ^ @c@ in wp
  | TStr -- ^ @c@ in wp
  | TNone -- ^ @c@ in wp
  | TVar TVar -- ^ @X@ in wp
  | TRec Rec -- ^ @t_r@ in wp
  | TOr (Set Type) -- ^ @t_b,1 v t_b,2@ in wp
  | TFieldUndefined -- ^ @_|_@ in wp
  | TFunc Constraints Env [Type] Type Constraints -- ^ @[t1, …, tn; Psi_1] => [tn+1; \Psi_2]@ in wp
  deriving (Eq, Ord)

-- * Record Type (@t_r = {l : t} | {}@ in wp)

-- | Record (models an object). A mapping from field names to Language.Types.
--
-- List of pairs @l : t@ in wp.
--
-- It is the same type as 'Env'
type Rec = Map Field Type

oneFieldTRec :: Field -> Type -> Rec
oneFieldTRec a t = Map.fromList [(a,t)]

emptyRecType :: Rec
emptyRecType = Map.empty


-- * Constraints (@Psi@ in wp)

-- | A mapping from Type Variable Names to Record Language.Types.
--
-- List of pairs @X <# t_r@ in wp.
type Constraints = Map TVar Rec

emptyConstraints :: Map.Map TVar Rec
emptyConstraints = Map.fromList []

-- * Environment: Variable Names to Language.Types (Sigma in wp)

-- | A mapping from Variable Names to Language.Types.
--
-- It is the same type as 'Env'
type Env = Map Var Type

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Var -> Type -> Env -> Env
extendEnv = Map.insert

-- * Locations to Type Variable Names (Gamma in wp)

data CheckState = CheckState { 
  _constraints :: Constraints,
  _freshInts :: [Int]
}
  deriving Eq

initState :: CheckState
initState = CheckState Map.empty [1..]
  
-- * Lenses

$(makeLens ''CheckState)

record :: TVar -> Lens CheckState Rec
record v = mapInsertLens v . constraints

field :: Field -> TVar -> Lens CheckState (Maybe Type)
field a v = mapLens a . record v

-- * Show instance

instance Show CheckState where
  show = showConstraints . getL constraints
  
showConstraints cs = concat ["[",showFields fields,"]"] where
  fields = Map.toList cs
  showFields fields = intercalate ", " (map showField fields)
  showField (l,t) = concat [l," < ", showRec t]

showRec :: Rec -> String 
showRec r = concat ["{",showFields fields,"}"] where
  fields = Map.toList r 
  showFields fields = intercalate ", " (map showField fields)
  showField (l,t) = concat [l,":",show t]
  
showEnv :: Env -> String
showEnv e = showPairs pairs where
  pairs = Map.toList e 
  showPairs pairs = intercalate ", " (map showPair pairs)
  showPair (l,t) = concat [l,":",show t]
  
instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show TStr  = "string"  
  show TNone = "NoneType"
  show (TVar v) = v
  show (TRec r) = showRec r
  show (TOr ts) = intercalate " v " $ map show $ Set.toList ts
  show (TFieldUndefined) = "undefined"
  show (TFunc constraintsBefore env paramTypes bodyType constraintsAfter) = 
    concat [showConstraints constraintsBefore,";",showEnv env, ";",
            intercalate " " (map show paramTypes), " -> ", 
            show bodyType, " ", showConstraints constraintsAfter
           ]

