{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.LexerUtils
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 06/12/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

module Language.R.LexerUtils (
    dblQuotedString,
    singQuotedString,
    quotedString
  ) where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Pos


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
