-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.Internal
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

module Language.R.Internal where

import Text.Parsec




-----------------------------------------------------------------------------
-- Basic data types
-----------------------------------------------------------------------------


-- |Internally, R normally uses doubles for storing all numeric values
-- (even when they are displayed as integers).  

data RNumType = RInt  -- ^Declared as integer
              | RNum  -- ^Declared as numeric
  deriving (Eq, Ord, Read, Show)

data RNumeric = RNumeric
  { rnVals   :: [Double]
  , decAs    :: RNumType }
  deriving (Eq, Ord, Read, Show)

-- |Character vectors are represented with standard Haskell Strings.

data RCharacter = RCharacter 
  { rcVals     :: [String] }
  deriving (Eq, Ord, Read, Show)


-- |Boolean vectors are represented with the standard Haskell Boolean.

data RBoolean = RBoolean 
  { rbVals   :: [Bool] }
  deriving (Eq, Ord, Read, Show)


-- data RVector = 


-- data RClosure = RClosure 
