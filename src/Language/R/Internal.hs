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
--    Core level internal definitions.
-----------------------------------------------------------------------------

module Language.R.Internal where

import Text.Parsec hiding (many, optional, (<|>))
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Pos
import Text.Parsec.Char (spaces)

import Control.Applicative
import Control.Monad.Identity

import Data.Either
import Debug.Trace
import Maybe


-----------------------------------------------------------------------------
-- Tokens
-----------------------------------------------------------------------------

-- |Parsing is performed using a two-pass lexer - parser sequence to generate
-- an abstract syntax tree.  This allows disambiguation and proper handling
-- of peculiarities of the R language.

type Token = (SourcePos, Tok)


-- |The LexState is simply the list of all processed tokens.
--
type LexState = [Tok]


-- |On first pass, tokens are identified without any lookahead based on
-- context alone. 

data Tok = StrTok String          -- ^String literal
         | NumTok String          -- ^Numeric literal (as a string)
         | AtmTok String          -- ^Textual atom (keyword, identifier, etc)
         | SymTok String          -- ^Non-alphanumeric character
         | ComTok String          -- ^Complete comment
  deriving (Eq, Ord, Show)

whiteSpace :: ParsecT String u Identity ()
whiteSpace =  spaces               -- ^TODO: This should be specified better

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
