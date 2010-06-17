-----------------------------------------------------------------------------
-- Module      : ShuntingYard
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 06/17/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

data Op = PLUS | MINUS | TIMES | DIVIDE | LPAREN | RPAREN | NEGATE
   
  deriving (Show, Ord, Read, Eq)

getPrec :: Op -> Int
getPrec a = 
  case a of 
      PLUS   -> 2
      MINUS  -> 1
      TIMES  -> 4
      DIVIDE -> 1
      LPAREN -> 0
      RPAREN -> 1
      NEGATE -> 5


