{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, CPP, DeriveDataTypeable #-}
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
--    Representation of the R Abstract Syntax Tree.
-----------------------------------------------------------------------------

module Language.R.AST where

import Language.R.Token
import Language.R.SrcLocation

import Data.Data


--------------------------------------------------------------------------------

-- | Convenient access to annotations in annotated types. 
class Annotated t where
   annot :: t annot -> annot

-- | Identifier.
data Ident annot = Ident 
  { ident_string :: String
  , ident_annot :: annot 
  } deriving (Eq, Ord, Show, Typeable, Data)

type IdentSpan = Ident SrcSpan

instance Span IdentSpan where
   getSpan = annot 

instance Annotated Ident where
   annot = ident_annot

-- | A block of statements. A suite is a group of statements controlled by a clause, 
-- for example, the body of a loop. 
--
--    * Version 2.6 <http://www.python.org/doc/2.6/reference/compound_stmts.html>
-- 
--    * Version 3.1 <http://www.python.org/doc/3.1/reference/compound_stmts.html>
--
{- 
type Suite annot = [Statement annot] 

type SuiteSpan = Suite SrcSpan
-}