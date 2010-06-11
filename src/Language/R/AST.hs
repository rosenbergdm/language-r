-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.AST
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 05/28/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

module Language.R.AST where

import Language.R.Internal

data RExpr = RNumeric
           | RCharacter
           | RLogical
           | ROperator
           | RFuncDef
           | RFuncProto
  deriving (Read, Show, Ord, Eq)


