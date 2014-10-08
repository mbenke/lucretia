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

import Lucretia.Language.Definitions ( IType )
import Control.Monad.State ( evalStateT, get, mzero, put, StateT )


type CM a = StateT CheckState [] a

evalCM :: CM a -> CheckState -> [a]
evalCM = evalStateT


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

-- ** Backtracking and errors (not in wp)

-- | For now error messages are ignored, since backtracking is handled by a 'List' monad and error is signalled there as a lack of a result in a list of results.
error :: String -> CM ()
error _ = mzero
