-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.Lexer
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 05/28/10
-- 
-- Description :
--    This module contains the functions used for lexing an list of
--    strings (representing either source code lines or interactive
--    input) into a stack of tokens.
-----------------------------------------------------------------------------

module Language.R.Lexer where

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


import Language.R.Internal
-- import Language.R.Generator


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
