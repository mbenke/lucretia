-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Basic definitions.
-----------------------------------------------------------------------------
module Lucretia.Language.Definitions (IRec, IVar, IAttr, IType, ErrorMsg) where

type Identifier = String

type IRec  = Identifier
type IVar  = Identifier
type IAttr = Identifier
type IType = Identifier

type ErrorMsg = String

