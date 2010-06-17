{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : ParserUtils.hs
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- created     : 06/14/10
-- 
-- Description :
--    Utilities for parsing a stream of lexed tokens into an AST.
-----------------------------------------------------------------------------

module Language.R.ParserUtils where


import qualified Data.Map as Map

import Language.R.Token
import Language.R.SrcLocation

import Data.List

sl = Sloc "" 1 1
ss = mkSrcSpan sl sl

data AssocDir = LR | RL 
  deriving (Show, Read, Eq, Ord)

type OpPrec = (Int, AssocDir)

contains :: String -> String -> Bool
contains st xs = any (\z -> st `isPrefixOf` z) (tails xs)

{- {{{ Operator precedence table 
-      {{{ From R Language Definition
0      ::
1      $ @
2      ^
3      - +                (unary)
4      :
5      %xyz%
6      * /
7      + -                (binary)
8      > >= < <= == !=
9      !
10    & &&
11    | ||
12     ~                  (unary and binary)
13     -> ->>
14     =                  (as assignment)
15     <- <<-
       }}} -}

class OpToken a where
  getOpPrecedence :: a -> OpPrec

instance OpToken Token where
  getOpPrecedence a = case a of 
    (ModulusToken _)        -> (5,  RL)
    (SlotOperator _)        -> (1, RL)
    (PlusToken _)           -> (3, LR)
    (NegateToken _)         -> (3, LR)
    (AssignLeftToken _)     -> (15, RL)
    (AssignRightToken _)    -> (15, LR)
    (AssignLeftToken' _)    -> (15, RL)
    (EqualsToken _)         -> (14, LR)
    (SliceReplaceToken _)   -> (15, LR)
    (MemberReplaceToken _)  -> (15, LR)
    (MemberReplaceToken' _) -> (15, LR)
    (MinusToken _)          -> (7, LR)
    (MultiplyToken _)       -> (6, LR)
    (DivideToken _)         -> (6, LR)
    (ExponentToken _)       -> (2, RL)
    (LessToken _)           -> (8, LR)
    (LessEqualToken _)      -> (8, LR)
    (EqualityToken _)       -> (8, LR)
    (InequalityToken _)     -> (8, LR)
    (GreatEqualToken _)     -> (8, LR)
    (GreatToken _)          -> (8, LR)
    (ElementwiseOrToken _)  -> (11, LR)
    (VectorOrToken _)       -> (11, LR)
    (ElementwiseAndToken _) -> (10, LR)
    (VectorAndToken _)      -> (10, LR)
    (SliceOperator _)       -> (0, LR)
    (MemberOperator _)      -> (0, LR)
    (MemberOperator' _)     -> (0, LR)
    _                       -> (-1, LR)

-- }}}


pop :: [a] -> a 
pop a =  last a

popped :: [a] -> [a]
popped a = init a

push :: [a] -> a -> [a]
push xs x = concat [xs, [x]]


runShunting :: [Token] -> [Token]
runShunting ins = runShunting' (ins, [], [])

runShunting' :: ([Token], [Token], [Token]) -> [Token]
runShunting' ([], outs, [])   = outs
runShunting' (ins, outs, ops) = 
  let (ins', outs', ops') = shuntStep ins outs ops
  in runShunting' (ins', outs', ops')

shuntStep :: [Token] -> [Token] -> [Token] -> ([Token], [Token], [Token])
shuntStep [] outs ops     = ( [], (concat [outs, [last ops]]), init ops)
shuntStep (x:xs) outs ops = 
  let (outs', ops') = case (classifyToken x) of
                        Value       -> shuntValue (x, outs, ops)
                        Builtin     -> shuntFunction (x, outs, ops)
                        Identifier  -> shuntIdentifier (x, outs, ops)
                        Punctuation -> shuntPunct (x, outs, ops)
                        Keyword     -> shuntFunction (x, outs, ops)
                        Operator    -> shuntOp (x, outs, ops)
                        Assignment  -> shuntFunction (x, outs, ops)
                        Replacement -> shuntFunction (x, outs, ops)
                        String      -> shuntValue (x, outs, ops)
                        Comment     -> (outs, ops)
  in (xs, outs', ops')


shuntOp :: (Token, [Token], [Token]) -> ([Token], [Token])
shuntOp (z, out, [])              = (concat [out, [z]], [])
shuntOp (z, out, ops@(y:ys))      =  
  if (snd op1, compare op1 op2) `elem` [(LR, LT), (LR, EQ), (RL, LT)] 
    then (shuntOp (z, (concat [out, [y]]), ys)) 
    else (concat [out, [z]], ys)
  where op1 = getOpPrecedence z
        op2 = getOpPrecedence y

shuntPunct :: (Token, [Token], [Token]) -> ([Token], [Token])
shuntPunct (p, out, ops) = 
  case p of 
    -- □ If the token is a left parenthesis, then push it onto the stack. 
    (ParenLeftToken _)  -> (out, concat [ops, [p]]) 
      
    -- □ If the token is a right parenthesis:
    --    ☆ Until the token at the top of the stack is a left parenthesis, pop
    --      operators off the stack onto the output queue.
    --    ☆ Pop the left parenthesis from the stack, but not onto the output
    --      queue.
    --    ☆ If the token at the top of the stack is a function token, pop it
    --      onto the output queue.
    --    ☆ If the stack runs out without finding a left parenthesis, then
    --      there are mismatched parentheses.
    (ParenRightToken _) ->
        let out1 = concat [out, takeWhile (\z -> tokenString z /= "(") (reverse ops)]
            ops1 = (reverse . (dropWhile (\z -> tokenString z /= "(")) . reverse) ops
            (out2, ops2) = case (isFunction $ last ops1) of 
                              True  -> (concat [out1, [last ops1]], (init ops1))
                              False -> (out1, ops1)
        in (out2, ops2)
    -- □ If the token is a function argument separator (e.g., a comma):
    --    ☆ Until the token at the top of the stack is a left parenthesis, pop
    --      operators off the stack onto the output queue. If no left
    --      parentheses are encountered, either the separator was misplaced or
    --      parentheses were mismatched.
    (CommaToken _)      -> 
        let out1 = concat [out, takeWhile (\z -> tokenString z /= "(") (reverse ops)]
            ops1 = (reverse . (dropWhile (\z -> tokenString z /= "(")) . reverse) ops
        in (out1, ops1)
    otherwise           -> (out, ops)                   


isFunction :: Token -> Bool
isFunction a = (classifyToken a) `elem` [Builtin, Replacement, Assignment]

data IdentifierType = FunctionID | ValueID deriving (Read, Show, Eq, Ord)

classifyIdentifier :: Token -> IdentifierType
classifyIdentifier a = case (contains "()" $ tokenString a) of
                           True  -> FunctionID
                           False -> ValueID
                          
shuntFunction :: (Token, [Token], [Token]) -> ([Token], [Token])
shuntFunction (z, out, ops) = (out, concat [ops, [z]])

shuntValue :: (Token, [Token], [Token]) -> ([Token], [Token])
shuntValue (z, out, ops) = (concat [out, [z]], ops)


shuntIdentifier :: (Token, [Token], [Token]) -> ([Token], [Token])
shuntIdentifier (z, out, ops) =
  case z' of
    FunctionID  -> shuntValue    (z, out, ops)
    ValueID     -> shuntFunction (z, out, ops)
-- ************************************************************
--                    TODO: What to do here
-- ************************************************************
    otherwise -> (out, ops)
  where z' = classifyIdentifier z


{- {{{ Shunting yard pseudocode
  • While there are tokens to be read:

      □ Read a token.
      □ If the token is a number, then add it to the output queue.
      X If the token is a function token, then push it onto the stack.
      X If the token is a function argument separator (e.g., a comma):

          ☆ Until the token at the top of the stack is a left parenthesis, pop
            operators off the stack onto the output queue. If no left
            parentheses are encountered, either the separator was misplaced or
            parentheses were mismatched.

      X If the token is an operator, o[1], then:

          ☆ while there is an operator token, o[2], at the top of the stack,
            and

                    either o[1] is left-associative and its precedence is less
                    than or equal to that of o[2],
                    or o[1] is right-associative and its precedence is less
                    than that of o[2],

                pop o[2] off the stack, onto the output queue;

          ☆ push o[1] onto the stack.

      X If the token is a left parenthesis, then push it onto the stack.
      X If the token is a right parenthesis:

          ☆ Until the token at the top of the stack is a left parenthesis, pop
            operators off the stack onto the output queue.
          ☆ Pop the left parenthesis from the stack, but not onto the output
            queue.
          ☆ If the token at the top of the stack is a function token, pop it
            onto the output queue.
          ☆ If the stack runs out without finding a left parenthesis, then
            there are mismatched parentheses.

  • When there are no more tokens to read:

      □ While there are still operator tokens in the stack:

          ☆ If the operator token on the top of the stack is a parenthesis,
            then there are mismatched parentheses.
          ☆ Pop the operator onto the output queue.

  • Exit.
}}} -}

myvals :: [Token]
myvals = [NumericToken ss "3.3" 3.3, PlusToken ss, NumericToken ss "5.1" 5.1, MultiplyToken ss, NumericToken ss "2.2" 2.2]
