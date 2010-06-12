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
import Control.Monad.Identity (Identity)
import Monad
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim
import Data.Char (toLower)
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L

{-
import Token
import SrcLocation
-}

import Language.R.Token
import Language.R.SrcLocation


--n| @Tok@s are the most general of the *R* token-type objects, representing a
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
{-
data TokType = LiteralConstant
             | Identifier
	     | ReservedWord
	     | SpecialOperator
	     | Separator
	     | OperatorToken
	     | GroupingToken
	     | IndexingToken



-}
data TokType = Variable
             | Reserved
             | Oprator
             | Bltin
             | Primitive
             | Strng
             | Intgr
             | Dble
             | CtrlFl
             | Grouping
             | Logical
             | Closure
             | Cmmnt
  deriving (Read, Eq, Ord)


instance Show TokType where
  show Variable  = "VARIABLE"
  show Reserved  = "RESERVED"
  show Oprator  = "OPERATOR"
  show Bltin   = "BUILTIN"
  show Primitive = "PRIMITIVE"
  show Strng     = "STRING"
  show Intgr     = "INTGR"
  show Dble      = "DBLE"
  show CtrlFl    = "CTRLFL"
  show Grouping  = "GROUPING"
  show Logical   = "LOGICAL"
  show Closure   = "CLOSURE"
  show Cmmnt   = "COMMENT"      -- ^ Section 10.2


-- | @Token@s are @Tok@s with the metadata of @SourcePos@
type TToken = (Tok, SourcePos)

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
  many lexRtok2

lexRtok2 = do
  pos <-  getPosition
  st  <-  getState
  tok <-  lexComment
      <|> lexSpecialConstant
      <|> lexInteger
      <|> lexComplex 
      <|> lexNumeric
      <|> lexLogical
      <|> lexString
      <|> lexOperator
      <|> lexReserved
--      <|> lexPunctuation
--      <|> lexIdentifier 
  return tok

lexSpecialConstant :: ParsecT String u Control.Monad.Identity.Identity Token
lexSpecialConstant = do
  startPos <- getPosition 
  tk <- choice [ try (string "NA_integer_")
               , try (string "NA_real_")
	       , try (string "NA_complex_")
	       , try (string "NA_character_")
	       , try (string "NA")
	       , try (string "NaN")
	       , try (string "NULL")
	       , (string "Inf") ]
  endPos <- getPosition
  return $ SpecialConstantToken (mkSrcSpan' startPos endPos) 
               tk (read tk :: SpecialConstant)


lexComment :: ParsecT String u Control.Monad.Identity.Identity Token
lexComment = do
  startPos <- getPosition 
  char '#'
  tk <-  many (noneOf "\n\r")
  endPos <- getPosition
  return $ CommentToken (mkSrcSpan' startPos endPos) tk 


lexInteger :: ParsecT String u Control.Monad.Identity.Identity Token
lexInteger = do
  startPos <- getPosition 
  tk <- many digit
  char 'L'
  endPos <- getPosition
  return $ IntegerToken (mkSrcSpan' startPos endPos) (tk ++ "L") (read tk :: Integer)
  

lexNumeric :: ParsecT String u Control.Monad.Identity.Identity Token
lexNumeric = do
  startPos <- getPosition 
  tk <- naturalOrFloat
  let tk' = either fromIntegral id tk :: Double
  endPos <- getPosition
  return $ NumericToken (mkSrcSpan' startPos endPos) (show tk) tk'

lexComplex :: ParsecT String u Control.Monad.Identity.Identity Token
lexComplex = do
  startPos <- getPosition
  (re', im') <- do 
    re <- naturalOrFloat
    char '+'
    im <- naturalOrFloat
    char 'i'
    let re' = either fromIntegral id re :: Double
    let im' = either fromIntegral id im :: Double
    return (re', im')  
  endPos <- getPosition
  return $ ComplexToken (mkSrcSpan' startPos endPos) 
                (show re' ++ " + " ++ show im' ++ "i") (re', im')


lexLogical :: ParsecT String u Control.Monad.Identity.Identity Token
lexLogical = do
  startPos <- getPosition 
  tk <- choice [ try (string "TRUE"), (string "FALSE") ]
  let tk' = read ((head tk) : (map Data.Char.toLower $ tail tk)) :: Bool 
  endPos <- getPosition
  return $ LogicalToken (mkSrcSpan' startPos endPos) tk tk' 


lexString :: ParsecT String u Control.Monad.Identity.Identity Token
lexString = do
  startPos <- getPosition 
  tk <- quotedString
  endPos <- getPosition
  return $ StringToken (mkSrcSpan' startPos endPos) tk 


lexOperator :: ParsecT String u Control.Monad.Identity.Identity Token
lexOperator = do
  startPos <- getPosition 
  let ops = map snd rResOps
  tk <- choice (map (\z -> try (string z)) ops)
  endPos <- getPosition
  let tk' = Map.lookup tk operatorMap
  res <- case tk' of 
              Just x  -> return x
              Nothing -> return ErrorToken
  return $ res (mkSrcSpan' startPos endPos) 


{-     <|> do 
           char '%'
           symb <- many alphaNum
           char '%'
           return $ concat ["%", symb, "%"]
-}

lexReserved :: ParsecT String u Control.Monad.Identity.Identity Token
lexReserved = do
  startPos <- getPosition 
  let rsrvd = map fst rFuncPrims
  tk <- choice (map (\z -> try (string z)) rsrvd)
  endPos <- getPosition
  let tk' = Map.lookup tk reservedMap
  res <- case tk' of 
              Just x  -> return x
              Nothing -> return ErrorToken
  return $ res (mkSrcSpan' startPos endPos) 

{-
lexIdentifier :: ParsecT String u Control.Monad.Identity.Identity Token
lexIdentifier = do
  startPos <- getPosition 
  let sp = Sloc (sourceName startPos) (sourceLine startPos) (sourceColumn startPos)
  tk <- identifier
  endPos <- getPosition
  let ep = Sloc (sourceName endPos) (sourceLine endPos) (sourceColumn endPos)
      tok_sp = mkSrcSpan sp ep
      tok = IdentifierToken tok_sp tk 
  return tok

lexPunctuation :: ParsecT String u Control.Monad.Identity.Identity Token
lexPunctuation = do
  startPos <- getPosition 
  let sp = Sloc (sourceName startPos) (sourceLine startPos) (sourceColumn startPos)

  endPos <- getPosition
  let ep = Sloc (sourceName endPos) (sourceLine endPos) (sourceColumn endPos)
      tok_sp = mkSrcSpan sp ep
      tok = PunctuationToken tok_sp tk 
  return tok
























-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- deprecated 
--
lexFloat = do
  tk <- float
  return $ RNumConst Dble tk

lexInt = do
  tk <- integer
  return $ RNumConst Intgr (fromIntegral tk)

lexSym = do
  syms <- many1 $ lexeme (oneOf ":!#$%&*+./<=>?@\\^|-~();,{}[]")
  let tk = elem syms $ map snd rResOps 
  return $ ROper Oprator syms

lexId = do
  tk <- identifier
  return $ RIdent Variable tk

lexComment = do
  char '#'
  tok <-  many (noneOf "\n\r")
  return $ RComment Cmmnt tok


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

-}


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

{-
testParse = do 
  text <- readFile "checkRegion.R"
  let res = runParser rTokenize [] "TEST" text
  return res


rLex text = 
  let res       = runParser rTokenize [] "rLex first pass" text
      tokStream = either (\z -> []) id res
  in tokStream
-}
