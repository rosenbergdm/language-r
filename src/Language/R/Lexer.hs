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

import Text.Parsec
import Text.Parsec.String
import Control.Monad


import Language.R.Internal
import Language.R.Generator


-- type TokParser = Parser RToken ()

{-
rTokenize :: (RToken -> Maybe a) -> TokParser a
rTokenize test = token showToken posToken testToken
  where showToken = tokLiteral
        posToken  = tokPos
        testToken = test
-}


-- rToks ::  TokParser [RToken] pos
rToks :: Parser [RToken]
rToks = many1 rToken

rToken = rString <|> rNumber <|> rReserved <|> rIdentifier


getStrTok = do
  lit <- rString
  return $ RToken StrTok lit

getNumTok = do
  num <- rNumber
  return $ RToken NumTok (show num)

-- getSymTok = do
--   opr <- rReservedOp <|> rOperator
--   return $ RToken SymTok (show opr)

getAtomTok = do
  atm <- rIdentifier <|> rReserved
  return $ RToken AtomTok (show atm)

-- rScanner :: [Char] -> ([Token], [String])

-- strTok :: TokParser String
-- strTok = rTokenize (\tok -> case tok of
                              


{- {{{

type Token  = (SourcePos,Tok)
data Tok    = Identifier String
            | Reserved   String
            | Symbol     String
            | Price      Int
            deriving Show

scanner :: [Char] -> ([Token],[String])
The parsers should now work on these token streams instead of the normal chararacter streams. This is reflected in the type. The type of general parsers is GenParser tok st a, where tok is the type of the tokens, st the type of the local user state and a is the type of the result. Indeed, Parser is just a type synonym for parsers that work on characters and have no state:

type Parser a   = GenParser Char () a
The token parser is used to define a parser that scans a single token. The function takes three arguments: a function to show tokens, a function to extract the source position of a token and finally a test function that determines if the token is accepted or not. The first two arguments are the same for all our tokens so we define a little abstraction:

type MyParser a   = GenParser Token () a

mytoken :: (Tok -> Maybe a) -> MyParser a
mytoken test
  = token showToken posToken testToken
  where
    showToken (pos,tok)   = show tok
    posToken  (pos,tok)   = pos
    testToken (pos,tok)   = test tok
Now, it is easy to define the basic parser that work on tokens.

}}} -}
{-
identifier :: MyParser String
identifier 
  = mytoken (\tok -> case tok of 
                       Identifier name -> Just name
                       other           -> Nothing)

reserved :: String -> MyParser ()
reserved name
  = mytoken (\tok -> case tok of
                       Reserved s   | s == name  -> Just ()
                       other        -> Nothing)

-}
