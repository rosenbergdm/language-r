{-# Language CPP, DeriveDataTypeable, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.Token
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 05/28/10
-- 
-- Description :
--    Necessary Token definitions for parsing the Splus / R Language.
-----------------------------------------------------------------------------

-- module Language.R.Token (
module Token (
  Token (..),

  tokenString,
  TokenClass (..),
  SpecialConstant (..),

  classifyToken
  ) where

import SrcLocation

import Data.Primitive.ByteArray (ByteArray)
import Data.Data
  
data SpecialConstant 
  = NA 
  | NaN 
  | NULL 
  | Inf 
  | TRUE 
  | FALSE 
  | NA_integer_
  | NA_real_
  | NA_complex_
  | NA_character_
    deriving (Read, Show, Eq, Ord)
  
-----------------------------------------------------------------------
-----------------------------------------------------------------------

-- | Lexical Tokens
data Token 
  -- Whitespace -------------------------------------------------------
  = SpaceToken { token_span :: SrcSpan }
  | NewlineToken { token_span :: SrcSpan }
  | TabToken { token_span :: SrcSpan }
  
  -- Comments ---------------------------------------------------------
  | CommentToken 
      { token_span    :: SrcSpan
      , token_literal :: String
      }
  
  -- Literals ---------------------------------------------------------
  ---- String Literal -----
  | StringToken
      { token_span    :: SrcSpan
      , token_literal :: String
      }

  ---- Numeric Literal ----
  | NumericToken
      { token_span     :: SrcSpan
      , token_literal  :: String
      , token_num      :: Double
      }

  ---- Logical Literal ----
  | LogicalToken
      { token_span     :: SrcSpan
      , token_literal  :: String
      , token_logical  :: Bool
      }
  
  ---- Integer Literal ----
  | IntegerToken
      { token_span     :: SrcSpan
      , token_literal  :: String
      , token_int      :: Integer
      }
  
  ---- Complex Literal ----
  | ComplexToken
      { token_span     :: SrcSpan
      , token_literal  :: String
      , token_comp     :: (Double, Double)
      }
  
  ---- Special Constants ----
  | SpecialConstantToken
      { token_span     :: SrcSpan
      , token_literal  :: String
      , token_special  :: SpecialConstant
      }

  -- Identifiers ------------------------------------------------------
  | IdentifierToken 
      { token_span    :: SrcSpan
      , token_literal :: String
      }
  
  -- Reserved Words --------------------------------------------------
  ---- Flow control ----
  | IfToken                     { token_span :: SrcSpan }   -- ^Keyword: \'if\'
  | ForToken                    { token_span :: SrcSpan }   -- ^Keyword: \'for\'
  | WhileToken                  { token_span :: SrcSpan }   -- ^Keyword: \'while\'
  | RepeatToken                 { token_span :: SrcSpan }   -- ^Keyword: \'repeat\'
  | ReturnToken                 { token_span :: SrcSpan }   -- ^Keyword: \'return\'
  | FunctionToken               { token_span :: SrcSpan }   -- ^Keyword: \'function\'
  | QuoteToken                  { token_span :: SrcSpan }   -- ^Keyword: \'quote\'
  | SwitchToken                 { token_span :: SrcSpan }   -- ^Keyword: \'switch\'
  | BreakToken                  { token_span :: SrcSpan }   -- ^Keyword: \'break\'
  | NextToken                   { token_span :: SrcSpan }   -- ^Keyword: \'next\'

  ---- Constants ----
  -- No need to list here, captured already


  -- Special operators ------------------------------------------------
  | MatrixMultiplyToken { token_span :: SrcSpan }     -- ^Operator: \'%*%\'
  | MatrixDivideToken { token_span :: SrcSpan }       -- ^Operator: \'%/%\'
  | IntersectToken { token_span :: SrcSpan }          -- ^Operator: \'%in%\'
  | OuterProductToken { token_span :: SrcSpan }       -- ^Operator: \'%o%\'
  | KroneckerProductToken { token_span :: SrcSpan }   -- ^Operator: \'%x%\'
  | CustomSpecialOpToken                              -- ^Operator: \'user defined\'
    { token_span    :: SrcSpan
    , token_literal :: String
    }

  -- Keywords
  -- Mathematical
  | AbsToken                    { token_span :: SrcSpan } -- ^Keyword: \'abs\'
  | SignToken                   { token_span :: SrcSpan } -- ^Keyword: \'sign\'
  | SqrtToken                   { token_span :: SrcSpan } -- ^Keyword: \'sqrt\'
  | FloorToken                  { token_span :: SrcSpan } -- ^Keyword: \'floor\'
  | CeilingToken                { token_span :: SrcSpan } -- ^Keyword: \'ceiling\'
  | ExpToken                    { token_span :: SrcSpan } -- ^Keyword: \'exp\'
  | Expm1Token                  { token_span :: SrcSpan } -- ^Keyword: \'expm1\'
  | Log2Token                   { token_span :: SrcSpan } -- ^Keyword: \'log2\'
  | Log10Token                  { token_span :: SrcSpan } -- ^Keyword: \'log10\'
  | Log1pToken                  { token_span :: SrcSpan } -- ^Keyword: \'log1p\'
  | CosToken                    { token_span :: SrcSpan } -- ^Keyword: \'cos\'
  | SinToken                    { token_span :: SrcSpan } -- ^Keyword: \'sin\'
  | TanToken                    { token_span :: SrcSpan } -- ^Keyword: \'tan\'
  | AcosToken                   { token_span :: SrcSpan } -- ^Keyword: \'acos\'
  | AsinToken                   { token_span :: SrcSpan } -- ^Keyword: \'asin\'
  | AtanToken                   { token_span :: SrcSpan } -- ^Keyword: \'atan\'
  | CoshToken                   { token_span :: SrcSpan } -- ^Keyword: \'cosh\'
  | SinhToken                   { token_span :: SrcSpan } -- ^Keyword: \'sinh\'
  | TanhToken                   { token_span :: SrcSpan } -- ^Keyword: \'tanh\'
  | AcoshToken                  { token_span :: SrcSpan } -- ^Keyword: \'acosh\'
  | AsinhToken                  { token_span :: SrcSpan } -- ^Keyword: \'asinh\'
  | AtanhToken                  { token_span :: SrcSpan } -- ^Keyword: \'atanh\'
  | GammaToken                  { token_span :: SrcSpan } -- ^Keyword: \'gamma\'
  | LgammaToken                 { token_span :: SrcSpan } -- ^Keyword: \'lgamma\'
  | DigammaToken                { token_span :: SrcSpan } -- ^Keyword: \'digamma\'
  | TrigammaToken               { token_span :: SrcSpan } -- ^Keyword: \'trigamma\'
  | CumsumToken                 { token_span :: SrcSpan } -- ^Keyword: \'cumsum\'
  | CumprodToken                { token_span :: SrcSpan } -- ^Keyword: \'cumprod\'
  | CummaxToken                 { token_span :: SrcSpan } -- ^Keyword: \'cummax\'
  | CumminToken                 { token_span :: SrcSpan } -- ^Keyword: \'cummin\'
  | ImToken                     { token_span :: SrcSpan } -- ^Keyword: \'Im\'
  | ReToken                     { token_span :: SrcSpan } -- ^Keyword: \'Re\'
  | ArgToken                    { token_span :: SrcSpan } -- ^Keyword: \'Arg\'
  | ConjToken                   { token_span :: SrcSpan } -- ^Keyword: \'Conj\'
  | ModToken                    { token_span :: SrcSpan } -- ^Keyword: \'Mod\'
  -- Metaprogramming / Language
  | LengthToken                 { token_span :: SrcSpan } -- ^Keyword: \'length\'
  | LengthAssignToken           { token_span :: SrcSpan } -- ^Keyword: \'length<-\'
  | ClassToken                  { token_span :: SrcSpan } -- ^Keyword: \'class\'
  | ClassAssignToken            { token_span :: SrcSpan } -- ^Keyword: \'class<-\'
  | OldClassToken               { token_span :: SrcSpan } -- ^Keyword: \'oldClass\'
  | OldCLassAssignToken         { token_span :: SrcSpan } -- ^Keyword: \'oldCLass<-\'
  | AttrToken                   { token_span :: SrcSpan } -- ^Keyword: \'attr\'
  | AttrAssignToken             { token_span :: SrcSpan } -- ^Keyword: \'attr<-\'
  | AttributesToken             { token_span :: SrcSpan } -- ^Keyword: \'attributes\'
  | AttributesAssignToken       { token_span :: SrcSpan } -- ^Keyword: \'attributes<-\'
  | NamesToken                  { token_span :: SrcSpan } -- ^Keyword: \'names\'
  | NamesAssignToken            { token_span :: SrcSpan } -- ^Keyword: \'names<-\'
  | DimToken                    { token_span :: SrcSpan } -- ^Keyword: \'dim\'
  | DimAssignToken              { token_span :: SrcSpan } -- ^Keyword: \'dim<-\'
  | DimnamesToken               { token_span :: SrcSpan } -- ^Keyword: \'dimnames\'
  | DimnamesAssignToken         { token_span :: SrcSpan } -- ^Keyword: \'dimnames<-\'
  | LevelsAssignToken           { token_span :: SrcSpan } -- ^Keyword: \'levels<-\'
  | EnvironmentAssignToken      { token_span :: SrcSpan } -- ^Keyword: \'environment<-\'
  | StorageModeAssignToken     { token_span :: SrcSpan }  -- ^Keyword: \'storage.mode<-\'
  
  -- Delimiters
  | SemicolonToken    { token_span :: SrcSpan }   -- ^Symbol: \';\'
  | CommaToken        { token_span :: SrcSpan }   -- ^Symbol: \',\'
  | ParenLeftToken    { token_span :: SrcSpan }   -- ^Symbol: \'(\'
  | ParenRightToken   { token_span :: SrcSpan }   -- ^Symbol: \')\'
  | BraceLeftToken    { token_span :: SrcSpan }   -- ^Symbol: \'{\'
  | BraceRightToken   { token_span :: SrcSpan }   -- ^Symbol: \'}\'
  | BracketLeftToken  { token_span :: SrcSpan }   -- ^Symbol: \'[\'
  | BracketRightToken { token_span :: SrcSpan }   -- ^Symbol: \']\'
  
  -- Category unclear
  | ThreeDotsToken    { token_span :: SrcSpan }   -- ^Symbol: \'...\'
  | ColonToken        { token_span :: SrcSpan }   -- ^Symbol: \':\'
  | DoubleColonToken  { token_span :: SrcSpan }   -- ^Symbol: \':::\'
  | QuestionMarkToken { token_span :: SrcSpan }   -- ^Symbol: \'?\'
  | DoubleQMarkToken  { token_span :: SrcSpan }   -- ^Symbol: \'??\'

  -- Operators 
  | ModulusToken { token_span :: SrcSpan }        -- ^Operator: \'%%\'
  | SlotOperator { token_span :: SrcSpan }        -- ^Operator: \'@\'
  | PlusToken { token_span :: SrcSpan }           -- ^Operator: \'+\'
  | NegateToken { token_span :: SrcSpan }         -- ^Operator: \'!\'
  | AssignLeftToken { token_span :: SrcSpan }     -- ^Operator: \'<-\'
  | AssignRightToken { token_span :: SrcSpan }    -- ^Operator: \'->\'
  | AssignLeftToken' { token_span :: SrcSpan }    -- ^Operator: \'<<-\'
  | EqualsToken { token_span :: SrcSpan }         -- ^Operator: \'=\'
  | SliceReplaceToken { token_span :: SrcSpan }   -- ^Operator: \'[<-\'
  | MemberReplaceToken { token_span :: SrcSpan }  -- ^Operator: \'[[<-\'
  | MemberReplaceToken' { token_span :: SrcSpan } -- ^Operator: \'$<-\'
  | MinusToken { token_span :: SrcSpan }          -- ^Operator: \'-\'
  | MultiplyToken { token_span :: SrcSpan }       -- ^Operator: \'*\'
  | DivideToken { token_span :: SrcSpan }         -- ^Operator: \'/\'
  | ExponentToken { token_span :: SrcSpan }       -- ^Operator: \'^\'
  | LessToken { token_span :: SrcSpan }           -- ^Operator: \'<\'
  | LessEqualToken { token_span :: SrcSpan }      -- ^Operator: \'<=\'
  | EqualityToken { token_span :: SrcSpan }       -- ^Operator: \'==\'
  | InequalityToken { token_span :: SrcSpan }     -- ^Operator: \'!=\'
  | GreatEqualToken { token_span :: SrcSpan }     -- ^Operator: \'>=\'
  | GreatToken { token_span :: SrcSpan }          -- ^Operator: \'>\'
  | ElementwiseOrToken { token_span :: SrcSpan }  -- ^Operator: \'|\'
  | VectorOrToken { token_span :: SrcSpan }       -- ^Operator: \'||\'
  | ElementwiseAndToken { token_span :: SrcSpan } -- ^Operator: \'&\'
  | VectorAndToken { token_span :: SrcSpan }      -- ^Operator: \'&&\'


  | SliceOperator  { token_span :: SrcSpan }      -- ^Operator: \'[\'
  | MemberOperator  { token_span :: SrcSpan }     -- ^Operator: \'[[\'
  | MemberOperator' { token_span :: SrcSpan }     -- ^Operator: \'$\'
  deriving (Show, Ord, Eq)


-- | TODO: These two data declartions need to go somewhere else
-- Numeric types
data RNumType = RNumType
  { num_typeof  :: String
  , num_vecmode :: RVectorMode
  } deriving (Read, Ord, Eq, Show)

-- |There are six types of vector storage in R
data RVectorMode 
  = VecLogical Bool
  | VecNumericInt Integer
  | VecNumeric Double
  | VecComplex (Double, Double)
  | VecCharacter Char
  | VecRaw String
  -- | VecRaw ByteArray 
  deriving (Read, Ord, Eq, Show)

-- | Classification of tokens
data TokenClass
   = Comment
   | Value
   | Identifier
   | Punctuation
   | Bracket
   | Keyword
   | String
   | Operator
   | Assignment
   | Replacement
   | Builtin
   deriving (Show, Eq, Ord)

classifyToken :: Token -> TokenClass
classifyToken token =
  case token of
      SpaceToken             {} -> Punctuation
      NewlineToken           {} -> Punctuation
      TabToken               {} -> Punctuation
      CommentToken           {} -> Comment
      StringToken            {} -> String
      NumericToken           {} -> Value
      LogicalToken           {} -> Value
      IdentifierToken        {} -> Identifier
      IfToken                {} -> Keyword
      ForToken               {} -> Keyword
      WhileToken             {} -> Keyword
      RepeatToken            {} -> Keyword
      ReturnToken            {} -> Keyword
      FunctionToken          {} -> Keyword
      QuoteToken             {} -> Keyword
      SwitchToken            {} -> Keyword
      BreakToken             {} -> Keyword
      NextToken              {} -> Keyword
      AbsToken               {} -> Builtin
      SignToken              {} -> Builtin
      SqrtToken              {} -> Builtin
      FloorToken             {} -> Builtin
      CeilingToken           {} -> Builtin
      ExpToken               {} -> Builtin
      Expm1Token             {} -> Builtin
      Log2Token              {} -> Builtin
      Log10Token             {} -> Builtin
      Log1pToken             {} -> Builtin
      CosToken               {} -> Builtin
      SinToken               {} -> Builtin
      TanToken               {} -> Builtin
      AcosToken              {} -> Builtin
      AsinToken              {} -> Builtin
      AtanToken              {} -> Builtin
      CoshToken              {} -> Builtin
      SinhToken              {} -> Builtin
      TanhToken              {} -> Builtin
      AcoshToken             {} -> Builtin
      AsinhToken             {} -> Builtin
      AtanhToken             {} -> Builtin
      GammaToken             {} -> Builtin
      LgammaToken            {} -> Builtin
      DigammaToken           {} -> Builtin
      TrigammaToken          {} -> Builtin
      CumsumToken            {} -> Builtin
      CumprodToken           {} -> Builtin
      CummaxToken            {} -> Builtin
      CumminToken            {} -> Builtin
      ImToken                {} -> Builtin
      ReToken                {} -> Builtin
      ArgToken               {} -> Builtin
      ConjToken              {} -> Builtin
      ModToken               {} -> Builtin
      LengthToken            {} -> Builtin
      LengthAssignToken      {} -> Replacement
      ClassToken             {} -> Builtin
      ClassAssignToken       {} -> Replacement
      OldClassToken          {} -> Builtin
      OldCLassAssignToken    {} -> Replacement
      AttrToken              {} -> Builtin
      AttrAssignToken        {} -> Replacement
      AttributesToken        {} -> Builtin
      AttributesAssignToken  {} -> Replacement
      NamesToken             {} -> Builtin
      NamesAssignToken       {} -> Replacement
      DimToken               {} -> Builtin
      DimAssignToken         {} -> Replacement
      DimnamesToken          {} -> Builtin
      DimnamesAssignToken    {} -> Replacement
      LevelsAssignToken      {} -> Replacement
      EnvironmentAssignToken {} -> Replacement
      StorageModeAssignToken {} -> Replacement
      SemicolonToken         {} -> Punctuation
      CommaToken             {} -> Punctuation
      ParenLeftToken         {} -> Punctuation
      ParenRightToken        {} -> Punctuation
      BraceLeftToken         {} -> Punctuation
      BraceRightToken        {} -> Punctuation
      BracketLeftToken       {} -> Punctuation
      BracketRightToken      {} -> Punctuation
      ThreeDotsToken         {} -> Keyword
      ColonToken             {} -> Builtin
      DoubleColonToken       {} -> Keyword
      QuestionMarkToken      {} -> Builtin
      DoubleQMarkToken       {} -> Builtin
      SliceOperator          {} -> Builtin
      MemberOperator         {} -> Builtin
      MemberOperator'        {} -> Builtin
      SlotOperator           {} -> Operator
      PlusToken              {} -> Operator
      NegateToken            {} -> Operator
      AssignLeftToken        {} -> Operator
      AssignRightToken       {} -> Operator
      AssignLeftToken'       {} -> Operator
      EqualsToken            {} -> Operator
      SliceReplaceToken      {} -> Operator
      MemberReplaceToken     {} -> Operator
      MemberReplaceToken'    {} -> Operator
      MinusToken             {} -> Operator
      MultiplyToken          {} -> Operator
      DivideToken            {} -> Operator
      ExponentToken          {} -> Operator
      ModulusToken           {} -> Operator
      MatrixMultiplyToken    {} -> Operator
      LessToken              {} -> Operator
      LessEqualToken         {} -> Operator
      EqualityToken          {} -> Operator
      InequalityToken        {} -> Operator
      GreatEqualToken        {} -> Operator
      GreatToken             {} -> Operator
      ElementwiseOrToken     {} -> Operator
      VectorOrToken          {} -> Operator
      ElementwiseAndToken    {} -> Operator
      VectorAndToken         {} -> Operator
      MatrixDivideToken      {} -> Operator



-- | Produce a string from a token 
tokenString :: Token -> String
tokenString token = 
   case token of
      SpaceToken             {} -> ""
      NewlineToken           {} -> ""
      TabToken               {} -> ""
      CommentToken           {} -> ""
      StringToken            {} -> token_literal token
      NumericToken           {} -> token_literal token
      LogicalToken           {} -> token_literal token
      IdentifierToken        {} -> token_literal token

      IfToken                {} -> "if" 
      ForToken               {} -> "for"
      WhileToken             {} -> "while"
      RepeatToken            {} -> "repeat"
      ReturnToken            {} -> "return"
      FunctionToken          {} -> "function"
      QuoteToken             {} -> "quote"
      SwitchToken            {} -> "switch"
      BreakToken             {} -> "break"
      NextToken              {} -> "next"
      AbsToken               {} -> "abs"
      SignToken              {} -> "sign"
      SqrtToken              {} -> "sqrt"
      FloorToken             {} -> "floor"
      CeilingToken           {} -> "ceiling"
      ExpToken               {} -> "exp"
      Expm1Token             {} -> "expm1"
      Log2Token              {} -> "log2"
      Log10Token             {} -> "log10"
      Log1pToken             {} -> "log1p"
      CosToken               {} -> "cos"
      SinToken               {} -> "sin"
      TanToken               {} -> "tan"
      AcosToken              {} -> "acos"
      AsinToken              {} -> "asin"
      AtanToken              {} -> "atan"
      CoshToken              {} -> "cosh"
      SinhToken              {} -> "sinh"
      TanhToken              {} -> "tanh"
      AcoshToken             {} -> "acosh"
      AsinhToken             {} -> "asinh"
      AtanhToken             {} -> "atanh"
      GammaToken             {} -> "gamma"
      LgammaToken            {} -> "lgamma"
      DigammaToken           {} -> "digamma"
      TrigammaToken          {} -> "trigamma"
      CumsumToken            {} -> "cumsum"
      CumprodToken           {} -> "cumprod"
      CummaxToken            {} -> "cummax"
      CumminToken            {} -> "cummin"
      ImToken                {} -> "Im"
      ReToken                {} -> "Re"
      ArgToken               {} -> "Arg"
      ConjToken              {} -> "Conj"
      ModToken               {} -> "Mod"

      LengthToken            {} -> "length"
      LengthAssignToken      {} -> "length<-"
      ClassToken             {} -> "class"
      ClassAssignToken       {} -> "class<-"
      OldClassToken          {} -> "oldClass"
      OldCLassAssignToken    {} -> "oldCLass<-"
      AttrToken              {} -> "attr"
      AttrAssignToken        {} -> "attr<-"
      AttributesToken        {} -> "attributes"
      AttributesAssignToken  {} -> "attributes<-"
      NamesToken             {} -> "names"
      NamesAssignToken       {} -> "names<-"
      DimToken               {} -> "dim"
      DimAssignToken         {} -> "dim<-"
      DimnamesToken          {} -> "dimnames"
      DimnamesAssignToken    {} -> "dimnames<-"
      LevelsAssignToken      {} -> "levels<-"
      EnvironmentAssignToken {} -> "environment<-"
      StorageModeAssignToken {} -> "storage.mode<-"

      SemicolonToken      {} -> ";"
      CommaToken          {} -> ","
      ParenLeftToken      {} -> "("
      ParenRightToken     {} -> ")"
      BraceLeftToken      {} -> "{"
      BraceRightToken     {} -> "}"
      BracketLeftToken    {} -> "["
      BracketRightToken   {} -> "]"
      ThreeDotsToken      {} -> "..."
      ColonToken          {} -> ":"
      DoubleColonToken    {} -> "::"
      QuestionMarkToken   {} -> "?"
      DoubleQMarkToken    {} -> "??"
      SliceOperator       {} -> "["
      MemberOperator      {} -> "[["
      MemberOperator'     {} -> "$"
      SlotOperator        {} -> "@"
      PlusToken           {} -> "+"
      NegateToken         {} -> "!"
      AssignLeftToken     {} -> "<-"
      AssignRightToken    {} -> "->"
      AssignLeftToken'    {} -> "<<-"
      EqualsToken         {} -> "="
      SliceReplaceToken   {} -> "[<-"
      MemberReplaceToken  {} -> "[[<-"
      MemberReplaceToken' {} -> "<-"
      MinusToken          {} -> "-"
      MultiplyToken       {} -> "*"
      DivideToken         {} -> "/"
      ExponentToken       {} -> "^"
      ModulusToken        {} -> "%%"
      MatrixMultiplyToken {} -> "%*%"
      LessToken           {} -> "<"
      LessEqualToken      {} -> "<="
      EqualityToken       {} -> "=="
      InequalityToken     {} -> "!="
      GreatEqualToken     {} -> ">="
      GreatToken          {} -> ">"
      ElementwiseOrToken  {} -> "|"
      VectorOrToken       {} -> "||"
      ElementwiseAndToken {} -> "&"
      VectorAndToken      {} -> "&&"
      MatrixDivideToken   {} -> "%/%"
      
