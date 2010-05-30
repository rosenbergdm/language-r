-----------------------------------------------------------------------------
-- |
-- Module      : Language.R
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 05/28/10
-- 
-- Description :
--    This will eventually be the command-line tool.
-----------------------------------------------------------------------------

-- module Language.R where
module Main where

import Language.R.Lexer
import Language.R.Parser
import Language.R.AST
import Language.R.Generator
import Language.R.Evaluate


-----------------------------------------------------------------------------
-- Top Level 
-----------------------------------------------------------------------------

main :: IO ()
main = putStrLn $ "The command-line tool has not been built yet."
