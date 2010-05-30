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
-- Tokens
-----------------------------------------------------------------------------

-- |Parsing is performed using a two-pass lexer - parser sequence to generate
-- an abstract syntax tree.  This allows disambiguation and proper handling
-- of peculiarities of the R language.

data Tok = StrTok    -- ^String Token
         | NumTok    -- ^Number Token
         | AtomTok   -- ^Atom Token
         | SymTok    -- ^Symbol Token
         | ComTok    -- ^Comment Token
  deriving (Eq, Ord, Show, Enum)


-- |Tokens are defined by their 'type', the literal string used to represent
-- them in the source code, and the location where they occured.

data RToken  =  RToken
  { tokType    :: Tok         -- ^Token type
  , tokLiteral :: String      -- ^The literal token string
--  , tokPos     :: SourcePos   -- ^The location (row, column) of the occurance
  } deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------
-- Basic data types
-----------------------------------------------------------------------------

-- |Internally, R normally uses doubles for storing all numeric values
-- (even when they are displayed as integers).  

data RNumType = RInt  -- ^Declared as integer
              | RNum  -- ^Declared as numeric
  deriving (Eq, Ord, Show)

data RNumeric = RNumeric
  { rnVals   :: [Double]
  , decAs    :: RNumType
  , rnNames  :: [String] }
  deriving (Eq, Ord, Show)

-- |Character vectors are represented with standard Haskell Strings.

data RCharacter = RCharacter 
  { rcVals     :: [String] 
  , rcNames    :: [String] }
  deriving (Eq, Ord, Show)


-- |Boolean vectors are represented with the standard Haskell Boolean.

data RBoolean = RBoolean 
  { rbVals   :: [Bool] 
  , rbNames  :: [String] }
  deriving (Eq, Ord, Show)

-- |Vectors form the most basic / atomic of R's data types
-- Vectors are required to be homogenous.
data RVector = Rnumeric [Double] RNumType (Maybe [String])
             | Rcharachter [String] (Maybe [String])
             | Rboolean [Bool] (Maybe [String])
  deriving (Eq, Ord, Show)


data RList = RList 
  { rlRows :: [RVector] }



-- data RClosure = RClosure 
