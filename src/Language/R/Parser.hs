-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.Parser
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

module Language.R.Parser where

import Text.Parsec.Pos


import Language.R.Internal
import Language.R.Lexer



-- |The expression and statement grammars for the R Language is given below
-- in tables below.
--
-- expr ::= var | const | (expr) | unop expr | expr duop expr 
--        | func ( {expr}+ )
-- var  ::= letter { letter | digit | _ | . | - }+ { idx }
-- idx  ::= @expr | $expr | [expr] | [[expr]]
-- unop ::= - | !
-- duop ::= + | - | * | / | ^ | & | && | '|' | '||' | %% | %in% 
--        | != | == | < | > | <= | >= | :
-- const::= num | TRUE | FALSE | NA | NULL | string
--
-- 
-- stmt ::= nop | var <- expr | if ( expr ) stmt { else stmt } 
--        | while ( expr ) stmt | for ( var in expr ) stmt 
--        | stmt {; stmt }+ | func2 ( {expr}+ )
--

-- |Data structures for describing the AST.

-- |Valid range for a lexically scoped variable.
-- TODO: This really should be implemented using a frame stack.
data Scope  = Scope 
  { sStart :: SourcePos          -- ^Start of scope
  , sEnd   :: SourcePos }        -- ^End of scope / GC okay
  deriving (Eq, Ord, Show)


-- |All numeric data types in R are actually represented as doubles; all R
-- 'atomic' datatypes are actually vectors.
data RNumeric = RNumeric 
  { rnVals    :: [Double]        -- ^Values
  , rnNames   :: Maybe [String]  -- ^Name vector
  , rnIdent   :: String          -- ^Bound symbol
  , rnScope   :: Scope           -- ^Valid scope
  , rnAlias   :: RNumType        -- ^Type as displayed by R
  } deriving (Eq, Ord, Show)

-- |Numeric variables in R can be declared as numeric() or integer()
data RNumType = NumTypeInteger | NumTypeNumeric
  deriving (Show, Ord, Eq, Enum)

-- |Character Vector
data RCharacter =  RCharacter
  { rcVals      :: [String]
  , rcNames     :: Maybe [String]
  , rcIdent     :: String
  , rcScope     :: Scope
  } deriving (Eq, Ord, Show)

-- |Boolean Vector
data RBoolean =  RBoolean
  { rbVals    :: [Bool]
  , rbNames   :: Maybe [String]
  , rbIdent   :: String
  , rbScope   :: Scope
  } deriving (Eq, Ord, Show)

-- |In general, and R Vector must be one of the following.
data RVector = VNumeric RNumeric 
             | VCharacter RCharacter 
             | VBool RBoolean
  deriving (Eq, Ord, Show)


data RList  =  RList
  { rlCols  :: [RObject]
  , rlNames :: Maybe [String]
  , rlScope :: Scope
  , rlIdent :: String
  } deriving (Eq, Ord, Show)

data RObject = OList RList
             | OBoolean RBoolean
             | OCharacter RCharacter
             | ONumeric RNumeric
  deriving (Eq, Ord, Show)


-- |From the manual R Internals / R Language Definition
-- 
--         ===================================
--         Six basic (atomic) types of vectors
--         =================================== 
--
--         typeof	mode	  storage.mode
--         -----------------------------------
--         logical	logical	  logical 
--         integer	numeric	  integer 
--         double	numeric	  double 
--         complex	complex	  complex 
--         character	character character 
--         raw	        raw	  raw 
--
--------------------------------------------------------------
--
--                  ==================
--                  Table of Operators
--                  ==================
--
--         -	Minus, can be unary or binary 
--         +	Plus, can be unary or binary 
--         !	Unary not 
--         ~	Tilde, used for model formulae, can be either unary or binary 
--         ?	Help 
--         :	Sequence, binary (in model formulae: interaction) 
--         *	Multiplication, binary 
--         /	Division, binary 
--         ^	Exponentiation, binary 
--         %x%	Special binary operators, x can be replaced by any valid name 
--         %%	Modulus, binary 
--         %/%	Integer divide, binary 
--         %*%	Matrix product, binary 
--         %o%	Outer product, binary 
--         %x%	Kronecker product, binary 
--         %in%	Matching operator, binary (in model formulae: nesting) 
--         <	Less than, binary 
--         >	Greater than, binary 
--         ==	Equal to, binary 
--         >=	Greater than or equal to, binary 
--         <=	Less than or equal to, binary 
--         &	And, binary, vectorized 
--         &&	And, binary, not vectorized 
--         |	Or, binary, vectorized 
--         ||	Or, binary, not vectorized 
--         <-	Left assignment, binary 
--         ->	Right assignment, binary 
--         $	List subset, binary 
-- 
-----------------------------------------------------------------
--
--             ======================================
--             Reserved / Builtin functions /keywords
--             ======================================
--
--                    -------------------
--                        **Special** 
--                    -------------------
--          {       (         if     for      while  repeat 
--          return  function  quote  switch   break  next
--
--
--                    -------------------
--                      **Language/Op** 
--                    -------------------
--
--               [    [[    $    @    +    !
--               <-   <<-  =    [<-  [[<-  $<-
--               -    *    /     ^    %%   %*%
--               <    <=   ==   !=    >=   >
--               |    ||   &    &&    !    %/%
--
--  
--                    -------------------
--                      **Language/Op** 
--                    -------------------
--
--                 abs     sign    sqrt
--                 floor   ceiling
--               
--                 exp     expm1
--                 log2    log10   log1p
--                 cos     sin     tan
--                 acos    asin    atan
--                 cosh    sinh    tanh
--                 acosh   asinh   atanh
--               
--                 gamma   lgamma  digamma trigamma
--                
--                 cumsum  cumprod cummax  cummin
--               
--                 Im  Re  Arg  Conj  Mod
--
--
--                    -------------------
--                      **Language/Op** 
--                    -------------------
--                    
--                    length      length<-
--                    class       class<-
--                    oldClass    oldCLass<-
--                    attr        attr<-
--                    attributes  attributes<-
--                    names       names<-
--                    dim         dim<-
--                    dimnames    dimnames<-
--                    levels<-    environment<-
--                                storage.mode<-
--
--


