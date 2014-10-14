-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- TypeChecker monad. The monad is serving two purposes:
-- * backtracking
-- * serving fresh 'IType' variable names.
-----------------------------------------------------------------------------
module Lucretia.TypeChecker.Monad where

import Lucretia.Language.Definitions ( IType, ErrorMsg )
import Control.Monad.State ( evalStateT, get, mzero, put, StateT )
import Control.Monad.Error ( runErrorT, throwError, ErrorT )
import Control.Monad.Identity ( runIdentity, Identity )

type CM = StateT CheckState (ErrorT ErrorMsg Identity)

evalCM :: CM a -> Either ErrorMsg a
evalCM m = runIdentity $ runErrorT $ evalStateT m initState


-- ** Fresh variables (not in wp)
-- New variable names are always fresh, thus there is no need
-- to check for name collisions.

-- | State, needed to serve fresh numbers.
type CheckState = [IType]

initState :: CheckState
initState = map (\c -> [c]) singleChar ++ map (\n -> "X"++show n) [1..]
  where
  singleChar = ['X'..'Z'] ++ ['A'..'W']

-- | Get fresh 'IType'
freshIType :: CM IType
freshIType = do
  current:next <- get
  put next
  return current

error :: String -> CM a
error = throwError

