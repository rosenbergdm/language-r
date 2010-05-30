-----------------------------------------------------------------------------
-- Module      : sandbox
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 05/29/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

import Text.Parsec hiding (many, optional, (<|>))
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Pos

import Control.Applicative
import Control.Monad.Identity

import Data.Either
import Debug.Trace
import Maybe

-- |On first pass, tokens are identified without any lookahead based on
-- context alone. 

type Token = (SourcePos, Tok)

-- |The LexState is simply the list of all processed tokens.
--
type LexState = [Tok]

data Tok = StrTok String          -- ^String literal
         | NumTok String          -- ^Numeric literal (as a string)
         | AtmTok String          -- ^Textual atom (keyword, identifier, etc)
         | SymTok String          -- ^Non-alphanumeric character
         | ComTok String          -- ^Complete comment
  deriving (Eq, Ord, Show)

whiteSpace = spaces               -- ^TODO: This should be specified better


-- | The 'top' token lexer function.  TODO: The sepBy clause is not correct.

rTokens :: ParsecT String LexState Identity [Token]
rTokens = 
  setState [] >>
  whiteSpace >>
  sepBy rToken spaces


-- | Lex a single token from the input stream and add it to the LexState
-- stack.

rToken :: ParsecT String LexState Identity Token
rToken = do
  sp <- getPosition
  st <- getState
  t <-  rComment
    <|> rString
    <|> rSymbol
    <|> rNumber
    <|> rIdent
  return (sp, t)


-- |Comments begin with an (unquoted) pound sign and continue until the
-- end of the line.

rComment :: ParsecT String LexState Identity Tok
rComment =  do
  char '#'
  content <- many1 (noneOf "\n\r")
  return $ ComTok content

-- |A string can be either single-quoted or double quoted and (within
-- the string) backslashes serve as escapes.
-- TODO: No escape function provided.
-- TODO: Single-quoting not implemented.

rString :: ParsecT String LexState Identity Tok
rString =  do 
  char '"'
  content <- many1 (noneOf "\"")
  char '"'
  return $ StrTok content


-- |A 'symbol' is any non-space, non-alphanumeric character.

rSymbol :: ParsecT String LexState Identity Tok
rSymbol =  do
  sym <- oneOf "~@%^&*_-+=$\\]}[{:?/><."
  return $ SymTok (sym:"")


-- |A number is lexed as a continuous list of digits.

rNumber :: ParsecT String LexState Identity Tok
rNumber =  do
  num <- (many1 digit) -- >> (optional (char '.' >> many digit)) 
  -- let num' = maybe "??" id num
  return $ NumTok num --'
  -- num <-  T.float <|> T.integer
  -- return $ liftM show num

-- |Atoms can only begin with a letter or a period.  After the first
-- character, any alphanumeric character or one of "-", "_", and "."
-- are permitted.

rIdent :: ParsecT String LexState Identity Tok
rIdent =  do
  first <- letter <|> (char '.')
  rest  <- many (noneOf " \t\n\r")
  --(letter <|> digit <|> (oneOf "._-"))
  return $ AtmTok ([first] ++ rest)



-- |lexRTextWithPos :: String -> Int -> Int -> String 
--                    -> Either ParseError [Token]
-- Given a source name (filename), start line, start
-- column, and 'content', lex a string into a list
-- of R tokens.

lexRTextWithPos f l c = do
  let sp = newPos f l c
  te <- readFile f
  let res = runParser rTokens [] f te
  return res
