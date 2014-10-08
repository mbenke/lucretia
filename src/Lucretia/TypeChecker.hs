-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- TypeChecker runner
-----------------------------------------------------------------------------
--module Lucretia.TypeChecker where
module Lucretia.TypeChecker ( typeProgramme ) where

import Control.Monad ( guard )
import Control.Monad.State ( lift )
import Data.Map as Map hiding ( update )

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types

import Lucretia.TypeChecker.Monad ( CM, evalCM, initState )
import Lucretia.TypeChecker.Renaming ( applyRenaming, getRenamingOnEnv )
import Lucretia.TypeChecker.Rules ( matchBlock )
import Lucretia.TypeChecker.Update ( update, extend )
import Lucretia.TypeChecker.Weakening ( weaker )


typeProgramme :: Defs -> ProgrammeType
typeProgramme b = evalCM (typeProgrammeM b) initState 

typeProgrammeM :: Defs -> CM (IType, Constraints)
typeProgrammeM b = do
  (id, PrePost pre post) <- matchBlock b emptyPrePost
  expectEmptyPreconditionsIn pre
  return (id, post)

    where expectEmptyPreconditionsIn pre =
            guard $ pre == emptyConstraints

