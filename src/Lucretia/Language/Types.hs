-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Types used in Lucretia TypeChecker and in function signature declarations.
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts #-}

module Lucretia.Language.Types where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Util.Debug

import Lucretia.Language.Definitions

import Data.Lens (Lens, mapLens, getL)
import Data.Lens.Template (makeLens)
import Prelude hiding ((.))
import Control.Category ((.))
import Util.MapLenses (mapInsertLens)

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Type-checker types (including constraints) and environment definitions.
-----------------------------------------------------------------------------
-- module Lucretia.Language.Types where

-- * Language.Types (/Defition 2.1 (Language.Types)/ in wp)
-- in one sentence: IAttr ~> TAttr = (Definedness, IType) ~> TOr = Map Kind TSingle

type ProgrammeType = [(IType, Constraints)]

type Type = (IType, PrePost)

-- | A mapping from type-variable names to types.
-- List of pairs @X <# t_r@ in wp.
type Constraints = Map IType TOr

-- Invariant: TOr can contain at most one TInt, one TString, TRec, ...
-- This is achieved by having a Map with Kind as the type of keys.
-- Invariant: TOr contains at least one Kind / single type.
type TOr         = Map Kind TSingle

data Kind        = KInt
                 | KString
                 | KBool
                 | KNone
                 | KRec
                 | KFun

                 deriving ( Eq, Ord, Show )

data TSingle     = TRec TRec
                 -- | TFunOr TFunOr --TODO intersection types
                 | TFun TFunSingle
                 | TPrimitive -- kind without parameters

                 deriving ( Eq, Ord, Show )

-- | Record (models an object). A mapping from attr names to Language.Types.
-- List of pairs @l : t@ in wp.
-- It is the same type as 'Env'
type TRec = Map IAttr TAttr

type TAttr = (Definedness, IType)

data Definedness = Required | Optional -- ^ that case is unneeded: | NotDefined
  deriving (Eq, Ord, Show)

type TFun = Maybe TFunSingle
--data TFun = Maybe TFunOr
--type TFunOr        = Set TFunSingle
data TFunSingle  = TFunSingle { funArgs :: [IType]
                              , funRet  :: IType
                              , funPP   :: PrePost
                              }
                 deriving ( Eq, Ord, Show )

data FunPrePost = InheritedPP | DeclaredPP PrePost

-- * Constraints (@Psi@ in wp)

-- | Pre- and post-Constraints (respectively: the left and the right
-- hand side of a type-checker rule / function signature).
data PrePost = PrePost { _pre  :: Constraints
                       , _post :: Constraints
                       }
                 deriving ( Eq, Ord, Show )

-- TODO RTR with Conditions in place of Constraints:
-- data Conditions = Conditions { _environment :: TRec
--                              , _constraints :: Constraints
--                              }
-- + no special handling in renaming
-- + no special entry in emptyConstraints
-- + no assertions in getEnv, fromEnv

$(makeLens ''PrePost)

emptyPrePost :: PrePost
emptyPrePost = PrePost emptyConstraints emptyConstraints

emptyConstraints :: Constraints
emptyConstraints = Map.singleton env $ tOrEmptyRec

-- showConstraints c = concat ["[",showAttrs attrs,"]"] where
--   attrs = Map.toList c
--   showAttrs attrs = intercalate ", " (map showAttr attrs)
--   showAttr (l,t) = concat [l," < ", showRec t]

-- * Record Type (@t_r = {l : t} | {}@ in wp)

-- showRec :: TRec -> String 
-- showRec r = concat ["{",showAttrs attrs,"}"] where
--   attrs = Map.toList r 
--   showAttrs attrs = intercalate ", " (map showAttr attrs)
--   showAttr (l,t) = concat [l,":",show t]

emptyRec :: TRec
emptyRec = Map.empty

env :: IType
env = "Env"

xId :: IType
xId = "xId"

aId :: IType
aId = "aId"

yId :: IType
yId = "yId"

fId :: IType
fId = "fId"

undefinedId :: IType
undefinedId = "undefinedId"

toEmptyRec id = (id, tOrEmptyRec)

tOrEmptyRec :: TOr
tOrEmptyRec = tOrFromTRec emptyRec

-- envToX :: IAttr -> (IType, TOr)
-- envToX x = toSingletonRec env x xId

toSingletonRec :: IType -> IAttr -> IType -> (IType, TOr)
toSingletonRec xId a aId = (xId, tOrSingletonRec a aId)

tOrSingletonRec :: IAttr -> IType -> TOr
tOrSingletonRec a t = tOrFromTRec $ Map.singleton a (Required, t)

tOrFromTSingle :: TSingle -> TOr
tOrFromTSingle tSingle = Map.singleton (kind tSingle) tSingle

tOrFromTRec :: TRec -> TOr
tOrFromTRec = tOrFromTSingle . TRec

tOrFromTFun :: TFunSingle -> TOr
tOrFromTFun = tOrFromTSingle . TFun

tOrFromTFunSingle :: TFunSingle -> TOr
tOrFromTFunSingle = tOrFromTSingle . TFun

-- | "env" type pointer is always present in Constraints and it is always a record
getEnv :: Constraints -> TRec
getEnv cs = tRec
  where TRec tRec = tOr Map.! KRec
        tOr = Map.findWithDefault tOrEmptyRec env cs

lookupInEnv :: IVar -> Constraints -> Maybe TAttr
lookupInEnv x cs = Map.lookup x $ getEnv cs

-- | Lookup 'TOr' for a given 'IType'.
--
-- The function will return the corresponding values as @('Just' value)@,
-- or 'Nothing' if 'IType' refers to a polymorphic type.
lookupInConstraints :: IType -> Constraints -> Maybe TOr
lookupInConstraints = Map.lookup

singletonConstraint :: IType -> TOr -> Constraints
singletonConstraint = Map.singleton

singletonTRec :: IAttr -> TAttr -> TRec
singletonTRec = Map.singleton

tOrPrimitive :: Kind -> TOr
tOrPrimitive kind = Map.singleton kind TPrimitive

kind :: TSingle -> Kind
kind (TRec _) = KRec
kind (TFun _) = KFun

-- * Show instance

showProgrammeType :: ProgrammeType -> String
showProgrammeType [] = "Programme does not type-check to any type"
showProgrammeType ts = intercalate " AND " $ map showSingleType ts
  where
  showSingleType (i, cs) = i++" with Constraints: "++showConstraints cs

showType :: Type -> String
showType (i, pp) = i++", "++showPrePost pp

showPrePost :: PrePost -> String
showPrePost (PrePost pre post) = concat
  [ "Pre="
  , showConstraints pre
  , " Post="
  , showConstraints post
  ]

showConstraints :: Constraints -> String
showConstraints cs = concat ["[", showFields cs, "]"]
  where
  showFields cs = intercalate ", " (map showField $ Map.toList cs)
  showField (l, t) = concat [l," < ", showTOr t]

showTOr :: TOr -> String
showTOr tOr = intercalate " v " $ map showKind $ Map.toList tOr

showKind :: (Kind, TSingle) -> String
showKind (KInt,    _) = "int"
showKind (KString, _) = "string"
showKind (KBool,   _) = "bool"
showKind (KNone,   _) = "None"
showKind (KRec, TRec t) = showRec t
showKind (KFun, TFun f) = showFun f
showKind other          = show other

showFun :: TFunSingle -> String
showFun (TFunSingle argIds returnId (PrePost pre post)) = concat
  [ "func ("
  , intercalate ", " argIds
  , ") "
  , showConstraints pre
  , " -> "
  , returnId
  , " "
  , showConstraints post
  ]

showRec :: TRec -> String
showRec r = concat ["{", showFields r, "}"]
  where
  showFields r = intercalate ", " $ map showField $ Map.toList r
  showField (a, (Required, i)) = concat [a, ": ", i]
  showField (a, (Optional, i)) = concat ["optional ", a, ": ", i]

