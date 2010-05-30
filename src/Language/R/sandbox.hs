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

type Token = (SourcePos, Tok)

type LexState = [Tok]

data Tok = StrTok String
         | NumTok String
--         | NumTok Float 
         | AtmTok String
         | SymTok String
         | ComTok String
  deriving (Eq, Ord, Show)

whiteSpace = spaces

rTokens :: ParsecT String LexState Identity [Token]
rTokens = 
  setState [] >>
  whiteSpace >>
  sepBy rToken spaces


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

rComment :: ParsecT String LexState Identity Tok
rComment =  do
  char '#'
  content <- many1 (noneOf "\n\r")
  return $ ComTok content

rString :: ParsecT String LexState Identity Tok
rString =  do 
  char '"'
  content <- many1 (noneOf "\"")
  char '"'
  return $ StrTok content

rSymbol :: ParsecT String LexState Identity Tok
rSymbol =  do
  sym <- oneOf "~@%^&*_-+=$\\]}[{:?/><."
  return $ SymTok (sym:"")

rNumber :: ParsecT String LexState Identity Tok
rNumber =  do
  num <- (many1 digit) -- >> (optional (char '.' >> many digit)) 
  -- let num' = maybe "??" id num
  return $ NumTok num --'
  -- num <-  T.float <|> T.integer
  -- return $ liftM show num

rIdent :: ParsecT String LexState Identity Tok
rIdent =  do
  first <- letter <|> (char '.')
  rest  <- many (noneOf " \t\n\r")
  --(letter <|> digit <|> (oneOf "._-"))
  return $ AtmTok ([first] ++ rest)

-- lexRTextWithPos :: String -> Int -> Int -> String 
--                    -> Either ParseError [Token]
lexRTextWithPos f l c = do
  let sp = newPos f l c
  te <- readFile f
  let res = runParser rTokens [] f te
  return res
