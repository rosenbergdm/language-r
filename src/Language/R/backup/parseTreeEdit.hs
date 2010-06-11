{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.ParseTreeEdit
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : Thu Jun 10 18:37:24 CDT 2010
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

module Language.R.ParseTreeEdit where

import Language.R.Generator

import Control.Monad
import Control.Monad.Error
import Monad
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L





data RFuncCall = RFuncCall
  { funcSymbol     :: RIdent
  , funcFormalArgs :: [(String, RExpr)]
  , funcOtherArgs  :: [RExpr]
  } deriving (Read, Show, Eq, Ord)


data RExpr = RFunc RFuncCall 
           | RVar RVariable
           | RConstVal RConstValue
           | RBlk RBlock
           | RId RIdent
  deriving (Read, Show, Ord, Eq)


binaryOpToFnCall :: RExpr -> ROper -> RExpr -> RExpr
binaryOpToFnCall x op y = 
  let fop = lookup rOpFunction op
  in RFuncCall fop [] [x, y]


rOpFunction :: Map.Map String RIdent
rOpFunction = Map.fromList 
  [ (ROper Operator "[",    RId "slice") ]
{-  , (ROper Operator "[[",   RIdent Primitive "element")
  , (ROper Operator "$",    RIdent Primitive "element'")
  , (ROper Operator "@",    RIdent Primitive "slot")
  , (ROper Operator "+",    RIdent Primitive "plus")
  , (ROper Operator "!",    RIdent Primitive "not")
  , (ROper Operator "<-",   RIdent Primitive "assignLeft")
  , (ROper Operator "<<-",  RIdent Primitive "assignLeft'")
  , (ROper Operator "=",    RIdent Primitive "equals")
  , (ROper Operator "[<-",  RIdent Primitive "replaceSlice")
  , (ROper Operator "[[<-", RIdent Primitive "replaceElement")
  , (ROper Operator "$<-",  RIdent Primitive "replaceElement'")
  , (ROper Operator "-",    RIdent Primitive "minus")
  , (ROper Operator "*",    RIdent Primitive "times")
  , (ROper Operator "/",    RIdent Primitive "divide")
  , (ROper Operator "^",    RIdent Primitive "exponent")
  , (ROper Operator "%%",   RIdent Primitive "modulus")
  , (ROper Operator "%*%",  RIdent Primitive "matrixMult")
  , (ROper Operator "<",    RIdent Primitive "lessThan")
  , (ROper Operator "<=",   RIdent Primitive "lessThanOrEqual")
  , (ROper Operator "==",   RIdent Primitive "isEqual")
  , (ROper Operator "!=",   RIdent Primitive "notEqual")
  , (ROper Operator ">=",   RIdent Primitive "greatThanOrEqual")
  , (ROper Operator ">",    RIdent Primitive "greaterThan")
  , (ROper Operator "|",    RIdent Primitive "or")
  , (ROper Operator "||",   RIdent Primitive "or'")
  , (ROper Operator "&",    RIdent Primitive "and")
  , (ROper Operator "&&",   RIdent Primitive "and'")
  , (ROper Operator "%/%",  RIdent Primitive "matrixDivide") ]
  -}
