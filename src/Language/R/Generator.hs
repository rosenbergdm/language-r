{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.Generator
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

module Language.R.Generator where


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



-- | @Tok@s are the most general of the *R* token-type objects, representing a
-- single *typeclass*-ish category of functionally similar language objects.
data Tok = RIdent TokType String    -- ^An @identifier@ i.e. variable
         | ROper TokType String     -- ^An @operator@ i.e. symbolic function name
         | RNumConst TokType Double -- ^All numbers are of type double
         | RSymb TokType String     -- ^A *punctuation* type character
         | RString TokType String   -- ^A literal @String@
         | RComment TokType String  -- ^A complete comment
  deriving (Read, Eq, Ord, Show)


-- | Objects of type @TokType@ represent the result of the first (lexing) pass of
-- the input text.  @TokType@s are identified without any lookahead or backtracing
-- of the input.
data TokType = Variable
             | Reserved
             | Operator
             | Builtin
             | Primitive
             | Strng
             | Intgr
             | Dble
             | CtrlFl
             | Grouping
             | Logical
             | Closure
             | Comment
  deriving (Read, Eq, Ord)

instance Show TokType where
  show Variable  = "VARIABLE"
  show Reserved  = "RESERVED"
  show Operator  = "OPERATOR"
  show Builtin   = "BUILTIN"
  show Primitive = "PRIMITIVE"
  show Strng     = "STRING"
  show Intgr     = "INTGR"
  show Dble      = "DBLE"
  show CtrlFl    = "CTRLFL"
  show Grouping  = "GROUPING"
  show Logical   = "LOGICAL"
  show Closure   = "CLOSURE"
  show Comment   = "COMMENT"


-- | @Token@s are @Tok@s with the metadata of @SourcePos@
type Token = (Tok, SourcePos)

-- | Objects of @TokSt@ are used to store the @Token@ stack during first pass
-- lexing.
type TokSt = [Tok]



-- | Reserved operations which cannot be overwritten and require rearrangement
-- during the second pass.
data R_ResOp  = BinMinus | BinPlus | UnNot | UnBinTilde 
              | UnHelp | BinSeq | BinMult | BinDiv | BinExp 
              | BinUndef | BinMod | BinIntDiv | BinMatMult 
              | BinOutPr | BinKronPr | BinIntrsct | BinLT 
              | BinGT | BinEQ | BinGE | BinLE | BinVecAnd 
              | BinAnd | BinVecOr | BinOr | BinAsnLeft 
              | BinAsnRight | BinElmnt | BinMemAsn
              | BinSlot | BinElemnt | BinElemntAsn 
              | BinIndxAsn | BinEql | BinIndx
  deriving (Read, Ord, Show, Eq, Enum)

-- | R *builtin* functions which cannot be overwritten and are not expressed
data R_Builtin = R_MathFn   -- ^Mathematical functions
               | R_PrgFn    -- ^Language / programming functions
               | R_DbgFn    -- ^Debugging functions
               | R_AsnFn    -- ^Special syntax assignment functions
               | R_nArgEff  -- ^Functions taking arbitrary length argument lists
               | R_CtrlFlow -- ^Control flow functions
  deriving (Read, Show, Eq, Ord, Enum)


rResOps :: [(R_ResOp, String)] 
rResOps  =  
  [ (BinMinus, "-")      -- ^ Minus, can be unary or binary
  , (BinPlus, "+")       -- ^ Plus, can be unary or binary
  , (UnNot, "!")         -- ^ Unary not
  , (UnBinTilde, "~")    -- ^ Tilde, used for model formulae, unary or binary
  , (UnHelp, "?")        -- ^ Help
  , (BinSeq, ":")        -- ^ Sequence, binary (in model formulae: interaction)
  , (BinMult, "*")       -- ^ Multiplication, binary
  , (BinDiv, "/")        -- ^ Division, binary
  , (BinExp, "^")        -- ^ Exponentiation, binary
  , (BinUndef, "%x%")    -- ^ Special binary operators, x can be replaced by any valid name
  , (BinMod, "%%")       -- ^ Modulus, binary
  , (BinIntDiv, "%/%")   -- ^ Integer divide, binary
  , (BinMatMult, "%*%")  -- ^ Matrix product, binary
  , (BinOutPr, "%o%")    -- ^ Outer product, binary
  , (BinKronPr, "%x%")   -- ^ Kronecker product, binary
  , (BinIntrsct, "%in%") -- ^ Matching operator, binary (in model formulae: nesting)
  , (BinLT, "<")         -- ^ Less than, binary
  , (BinGT, ">")         -- ^ Greater than, binary
  , (BinEQ, "==")        -- ^ Equal to, binary
  , (BinGE, ">=")        -- ^ Greater than or equal to, binary
  , (BinLE, "<=")        -- ^ Less than or equal to, binary
  , (BinVecAnd, "&")     -- ^ And, binary, vectorized
  , (BinAnd, "&&")       -- ^ And, binary, not vectorized
  , (BinVecOr, "| ")     -- ^ Or, binary, vectorized
  , (BinOr, "||")        -- ^ Or, binary, not vectorized
  , (BinAsnLeft, "<-")   -- ^ Left assignment, binary
  , (BinAsnRight, "->")  -- ^ Right assignment, binary
  , (BinElmnt, "$")      -- ^ List subset, binary
  , (BinMemAsn, "$<-")
  , (BinSlot, "@")
  , (BinIndx, "[")
  , (BinElemnt, "[[")
  , (BinElemntAsn, "[[<-")
  , (BinIndxAsn, "[<-")
  , (BinEql, "=")
  ]

resOps :: Map.Map R_ResOp String
resOps  = Map.fromList rResOps

rFuncPrims :: [(String, R_Builtin)]
rFuncPrims  =  
  [ ("abs"                 , R_MathFn)
  , ("sign"                , R_MathFn)
  , ("sqrt"                , R_MathFn)
  , ("floor"               , R_MathFn)
  , ("ceiling"             , R_MathFn)
  , ("exp"                 , R_MathFn)
  , ("expm1"               , R_MathFn)
  , ("log2"                , R_MathFn)
  , ("log10"               , R_MathFn)
  , ("log1p"               , R_MathFn)
  , ("cos"                 , R_MathFn)
  , ("sin"                 , R_MathFn)
  , ("tan"                 , R_MathFn)
  , ("acos"                , R_MathFn)
  , ("asin"                , R_MathFn)
  , ("atan"                , R_MathFn)
  , ("cosh"                , R_MathFn)
  , ("sinh"                , R_MathFn)
  , ("tanh"                , R_MathFn)
  , ("acosh"               , R_MathFn)
  , ("asinh"               , R_MathFn)
  , ("atanh"               , R_MathFn)
  , ("gamma"               , R_MathFn)
  , ("lgamma"              , R_MathFn)
  , ("digamma"             , R_MathFn)
  , ("trigamma"            , R_MathFn)
  , ("cumsum"              , R_MathFn)
  , ("cumprod"             , R_MathFn)
  , ("cummax"              , R_MathFn)
  , ("cummin"              , R_MathFn)
  , ("Im"                  , R_MathFn)
  , ("Re"                  , R_MathFn)
  , ("Arg"                 , R_MathFn)
  , ("Conj"                , R_MathFn)
  , ("Mod"                 , R_MathFn)
  , ("nargs"               , R_PrgFn)
  , ("missing"             , R_PrgFn)
  , ("interactive"         , R_PrgFn)
  , ("is.xxx"              , R_PrgFn)
  , ("as.call"             , R_PrgFn)
  , ("as.character"        , R_PrgFn)
  , ("as.complex"          , R_PrgFn)
  , ("as.double"           , R_PrgFn)
  , ("as.environment"      , R_PrgFn)
  , ("as.integer"          , R_PrgFn)
  , ("as.logical   as.raw" , R_PrgFn)
  , (".Primitive"          , R_PrgFn)
  , (".Internal"           , R_PrgFn)
  , ("globalenv"           , R_PrgFn)
  , ("baseenv"             , R_PrgFn)
  , ("emptyenv"            , R_PrgFn)
  , ("pos.to.env"          , R_PrgFn)
  , ("unclass"             , R_PrgFn)
  , ("invisible"           , R_PrgFn)
  , ("seq_along"           , R_PrgFn)
  , ("seq_len"             , R_PrgFn)
  , ("browser"             , R_DbgFn)
  , ("proc.time"           , R_DbgFn)
  , ("gc.time"             , R_DbgFn)
  , ("tracemem"            , R_DbgFn)
  , ("retracemem"          , R_DbgFn)
  , ("untracemem"          , R_DbgFn)
  , ("length"              , R_AsnFn)
  , ("length<-"            , R_AsnFn)
  , ("class"               , R_AsnFn)
  , ("class<-"             , R_AsnFn)
  , ("oldClass"            , R_AsnFn)
  , ("oldClass<-"          , R_AsnFn)
  , ("attr"                , R_AsnFn)
  , ("attr<-"              , R_AsnFn)
  , ("attributes"          , R_AsnFn)
  , ("attributes<-"        , R_AsnFn)
  , ("names"               , R_AsnFn)
  , ("names<-"             , R_AsnFn)
  , ("dim"                 , R_AsnFn)
  , ("dim<-"               , R_AsnFn)
  , ("dimnames"            , R_AsnFn)
  , ("dimnames<-"          , R_AsnFn)
  , ("environment<-"       , R_AsnFn)
  , ("levels<-"            , R_AsnFn)
  , ("storage.mode<-"      , R_AsnFn)
  -- , (":"                   , R_nArgEff)
  -- , ("~"                   , R_nArgEff)
  , ("c"                   , R_nArgEff)
  , ("list"                , R_nArgEff)
  , ("call"                , R_nArgEff)
  , ("expression"          , R_nArgEff)
  , ("substitute"          , R_nArgEff)
  , ("UseMethod"           , R_nArgEff)
  , ("standardGeneric"     , R_nArgEff)
  , (".C"                  , R_nArgEff)
  , (".Fortran"            , R_nArgEff)
  , (".Call"               , R_nArgEff)
  , (".External"           , R_nArgEff)
  , (".Call.graphics"      , R_nArgEff)
  , (".External.graphics"  , R_nArgEff)
  , (".subset"             , R_nArgEff)
  , (".subset2"            , R_nArgEff)
  , (".primTrace"          , R_nArgEff)
  , (".primUntrace"        , R_nArgEff)
  , ("round"               , R_nArgEff)
  , ("signif"              , R_nArgEff)
  , ("rep"                 , R_nArgEff)
  , ("seq.int"             , R_nArgEff)
  , ("lazyLoadDBfetch"     , R_nArgEff)
  , ("break"               , R_CtrlFlow)
  , ("for"                 , R_CtrlFlow)
  , ("function"            , R_CtrlFlow)
  , ("if"                  , R_CtrlFlow)
  , ("next"                , R_CtrlFlow)
  , ("repeat"              , R_CtrlFlow)
  , ("return"              , R_CtrlFlow)
  , ("while"               , R_CtrlFlow)
  ]


funcPrims :: Map.Map String R_Builtin
funcPrims  = Map.fromList rFuncPrims


rStyle :: L.LanguageDef st
rStyle = L.emptyDef
   { T.commentStart   = ""
   , T.commentEnd     = ""
   , T.commentLine    = "#"
   , T.nestedComments = False
   , T.identStart     = letter <|> char '.'
   , T.identLetter    = alphaNum <|> oneOf "._"
   , T.opStart        = T.opLetter L.emptyDef
   , T.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~(){}[];,"
   , T.reservedOpNames= []  -- map snd rResOps
   , T.reservedNames  = []  -- map fst rFuncPrims
   , T.caseSensitive  = True
   }


rlang :: T.TokenParser st
rlang =  T.makeTokenParser rStyle

 

-- For ease of export
identifier     = T.identifier     rlang 
reserved       = T.reserved       rlang 
operator       = T.operator       rlang 
reservedOp     = T.reservedOp     rlang 
charLiteral    = T.charLiteral    rlang 
stringLiteral  = T.stringLiteral  rlang 
natural        = T.natural        rlang 
integer        = T.integer        rlang 
float          = T.float          rlang 
naturalOrFloat = T.naturalOrFloat rlang 
decimal        = T.decimal        rlang 
hexadecimal    = T.hexadecimal    rlang 
octal          = T.octal          rlang 
symbol         = T.symbol         rlang 
lexeme         = T.lexeme         rlang 
whiteSpace     = T.whiteSpace     rlang 
parens         = T.parens         rlang 
braces         = T.braces         rlang 
angles         = T.angles         rlang 
brackets       = T.brackets       rlang 
squares        = T.squares        rlang 
semi           = T.semi           rlang 
comma          = T.comma          rlang 
colon          = T.colon          rlang 
dot            = T.dot            rlang 
semiSep        = T.semiSep        rlang 
semiSep1       = T.semiSep1       rlang 
commaSep       = T.commaSep       rlang 
commaSep1      = T.commaSep1      rlang 




-- rTokenize :: ParsecT String TokSt Identity [Token]
rTokenize  = 
  setState [] >>
  whiteSpace >>
  many lexRtok

lexRtok  = do
  pos <-  getPosition
  st  <-  getState
  tok <-  lexComment
      <|> lexInt
      <|> lexFloat
      <|> lexString
      <|> lexSym
      <|> lexId
  return (tok, pos)


lexFloat = do
  tk <- float
  return $ RNumConst Dble tk

lexInt = do
  tk <- integer
  return $ RNumConst Intgr (fromIntegral tk)

lexSym = do
  syms <- many1 $ lexeme (oneOf ":!#$%&*+./<=>?@\\^|-~();,{}[]")
  let tk = elem syms $ map snd rResOps 
  return $ ROper Operator syms

lexId = do
  tk <- identifier
  return $ RIdent Variable tk

lexComment = do
  char '#'
  tok <-  many (noneOf "\n\r")
  return $ RComment Comment tok

lexString = do
  tok <- quotedString
  return $ RString Strng tok 
          

dblQuotedString :: (Text.Parsec.Prim.Stream s m Char) => Text.Parsec.Prim.ParsecT s u m String
dblQuotedString = do
  capture <- (between (char '"') (char '"') (many $ choice [char '\\' >> char '"', noneOf "\""]))
  return $ capture

singQuotedString :: (Text.Parsec.Prim.Stream s m Char) => Text.Parsec.Prim.ParsecT s u m String
singQuotedString = do
  capture <- (between (char '\'') (char '\'') (many $ choice [char '\\' >> char '\'', noneOf "'"]))
  return $ capture


quotedString :: (Text.Parsec.Prim.Stream s m Char) => Text.Parsec.Prim.ParsecT s u m String
quotedString =  try dblQuotedString
            <|> singQuotedString


testParse = do 
  text <- readFile "checkRegion.R"
  let res = runParser rTokenize [] "TEST" text
  return res


rLex text = 
  let res       = runParser rTokenize [] "rLex first pass" text
      tokStream = either (\z -> []) id res
  in tokStream

