{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- Module      : lexString
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 06/10/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim


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
