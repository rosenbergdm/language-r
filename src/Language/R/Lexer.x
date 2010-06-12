{
-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.Alex.Lexer
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 06/11/2010
-- 
-- Description :
--    Special purpose token lexing functions for use through the alex
--    lexer generator
-----------------------------------------------------------------------------

module Language.Python.Version2.Parser.Lexer where

import Language.R.Token as Token
import Language.R.SrcLocation


import qualified Data.Map as Map
import Control.Monad (liftM)
import Data.List (foldl')
import Numeric (readHex, readOct)
}

-- character sets
$lf = \n  -- line feed
$cr = \r  -- carriage return
$eol_char = [$lf $cr] -- any end of line character
$not_eol_char = ~$eol_char -- anything but an end of line character
$white_char   = [\ \n\r\f\v\t]
$white_no_nl = $white_char # $eol_char
$ident_letter = [a-zA-Z_]
$digit    = 0-9
$non_zero_digit = 1-9
$oct_digit = 0-7
$hex_digit = [$digit a-fA-F]
$bin_digit = 0-1 
$short_str_char = [^ \n \r ' \" \\]
$long_str_char = [. \n] # [' \"]
$short_byte_str_char = \0-\127 # [\n \r ' \" \\]
$long_byte_str_char = \0-\127 # [' \"]
$not_single_quote = [. \n] # '
$not_double_quote = [. \n] # \"

-- macro definitions
@exponent = (e | E) (\+ | \-)? $digit+ 
@fraction = \. $digit+
@int_part = $digit+
@point_float = (@int_part? @fraction) | @int_part \.
@exponent_float = (@int_part | @point_float) @exponent
@float_number = @point_float | @exponent_float
@eol_pattern = $lf | $cr $lf | $cr $lf  
@one_single_quote = ' $not_single_quote
@two_single_quotes = '' $not_single_quote
@one_double_quote = \" $not_double_quote
@two_double_quotes = \"\" $not_double_quote
@byte_str_prefix = b | B
@raw_str_prefix = r | R
@raw_byte_str_prefix = @byte_str_prefix @raw_str_prefix
@backslash_pair = \\ (\\|'|\"|@eol_pattern|$short_str_char)
@backslash_pair_bs = \\ (\\|'|\"|@eol_pattern|$short_byte_str_char)
@short_str_item_single = $short_str_char|@backslash_pair|\"
@short_str_item_double = $short_str_char|@backslash_pair|'
@short_byte_str_item_single = $short_byte_str_char|@backslash_pair_bs|\"
@short_byte_str_item_double = $short_byte_str_char|@backslash_pair_bs|'
@long_str_item_single = $long_str_char|@backslash_pair|@one_single_quote|@two_single_quotes|\"
@long_str_item_double = $long_str_char|@backslash_pair|@one_double_quote|@two_double_quotes|'
@long_byte_str_item_single = $long_byte_str_char|@backslash_pair_bs|@one_single_quote|@two_single_quotes|\"
@long_byte_str_item_double = $long_byte_str_char|@backslash_pair_bs|@one_double_quote|@two_double_quotes|'

tokens :-

-- these rules below could match inside a string literal, but they
-- will not be applied because the rule for the literal will always
-- match a longer sequence of characters. 


<0> $ident_letter($ident_letter|$digit)*  { \loc len str -> keywordOrIdent (take len str) loc }

-- operators and separators
--
<0> {
    "("   { openParen LeftRoundBracketToken }
    ")"   { closeParen RightRoundBracketToken }
    "["   { openParen LeftSquareBracketToken }
    "]"   { closeParen RightSquareBracketToken }
    "{"   { openParen LeftBraceToken }
    "}"   { closeParen RightBraceToken }
    "->"  { symbolToken RightArrowToken }
    "."   { symbolToken DotToken }
    "..." { symbolToken EllipsisToken }
    "~"   { symbolToken TildeToken }
    "+"   { symbolToken PlusToken }
    "-"   { symbolToken MinusToken }
    "**"  { symbolToken ExponentToken }
    "*"   { symbolToken MultToken }
    "/"   { symbolToken DivToken }
    "//"  { symbolToken FloorDivToken }
    "%"   { symbolToken ModuloToken }
    "<<"  { symbolToken ShiftLeftToken }
    ">>"  { symbolToken ShiftRightToken }
    "<"   { symbolToken LessThanToken }
    "<="  { symbolToken LessThanEqualsToken }
    ">"   { symbolToken GreaterThanToken }
    ">="  { symbolToken GreaterThanEqualsToken }
    "=="  { symbolToken EqualityToken }
    "!="  { symbolToken NotEqualsToken }
    "<>"  { symbolToken NotEqualsV2Token } -- only version 2
    "^"   { symbolToken XorToken }
    "|"   { symbolToken BinaryOrToken }
    "&&"  { symbolToken AndToken }
    "&"   { symbolToken BinaryAndToken }
    "||"  { symbolToken OrToken }
    ":"   { symbolToken ColonToken }
    "="   { symbolToken AssignToken }
    "+="  { symbolToken PlusAssignToken }
    "-="  { symbolToken MinusAssignToken }
    "*="  { symbolToken MultAssignToken }
    "/="  { symbolToken DivAssignToken }
    "%="  { symbolToken ModAssignToken }
    "**=" { symbolToken PowAssignToken }
    "&="  { symbolToken BinAndAssignToken }
    "|="  { symbolToken BinOrAssignToken }
    "^="  { symbolToken BinXorAssignToken }
    "<<=" { symbolToken LeftShiftAssignToken }
    ">>=" { symbolToken RightShiftAssignToken }
    "//=" { symbolToken FloorDivAssignToken } 
    ","   { symbolToken CommaToken }
    "@"   { symbolToken AtToken }
    \;    { symbolToken SemiColonToken }
    "`"   { symbolToken BackQuoteToken }
}

{- 
-- The lexer starts off in the beginning of file state (bof)
initStartCodeStack :: [Int]
initStartCodeStack = [bof,0]

lexToken :: P Token
lexToken = do
  location <- getLocation
  input <- getInput
  startCode <- getStartCode
  case alexScan (location, input) startCode of
    AlexEOF -> do
       depth <- getIndentStackDepth
       if depth <= 1
          then return endOfFileToken
          else do
             popIndent
             return dedentToken
    AlexError _ -> lexicalError
    AlexSkip (nextLocation, rest) len -> do
       setLocation nextLocation
       setInput rest
       lexToken
    AlexToken (nextLocation, rest) len action -> do
       setLocation nextLocation
       setInput rest
       token <- action (mkSrcSpan location $ decColumn 1 nextLocation) len input
       setLastToken token
       return token

-- This is called by the Happy parser.
lexCont :: (Token -> P a) -> P a
lexCont cont = do
   lexLoop
   where
   -- lexLoop :: P a
   lexLoop = do
      tok <- lexToken
      case tok of
         CommentToken {} -> do
            addComment tok
            lexLoop
         LineJoinToken {} -> lexLoop
         _other -> cont tok

-- a keyword or an identifier (the syntax overlaps)
keywordOrIdent :: String -> SrcSpan -> P Token
keywordOrIdent str location
   = return $ case Map.lookup str keywords of
         Just symbol -> symbol location
         Nothing -> IdentifierToken location str

-- mapping from strings to keywords
keywords :: Map.Map String (SrcSpan -> Token)
keywords = Map.fromList keywordNames

-- see: <http://docs.python.org/reference/lexical_analysis.html#keywords>
keywordNames :: [(String, SrcSpan -> Token)]
keywordNames =
   [ ("and", AndToken), ("as", AsToken), ("assert", AssertToken), ("break", BreakToken)
   , ("class", ClassToken), ("continue", ContinueToken), ("def", DefToken), ("del", DeleteToken)
   , ("elif", ElifToken),  ("else", ElseToken), ("except", ExceptToken), ("exec", ExecToken)
   , ("finally", FinallyToken), ("for", ForToken), ("from", FromToken),  ("global", GlobalToken)
   , ("if", IfToken), ("import", ImportToken), ("in", InToken), ("is", IsToken)
   , ("lambda", LambdaToken), ("not", NotToken), ("or", OrToken), ("pass", PassToken)
   , ("print", PrintToken), ("raise", RaiseToken), ("return", ReturnToken), ("try", TryToken)
   , ("while", WhileToken), ("with", WithToken), ("yield", YieldToken)
   ]
}
-}