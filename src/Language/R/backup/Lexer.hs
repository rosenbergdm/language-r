{-# OPTIONS -cpp #-}
{-# LINE 1 "Lexer.x" #-}

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

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
alex_base :: Array Int Int
alex_base = listArray (0,53) [-32,47,122,0,0,0,0,0,0,0,-59,-42,0,-39,0,-45,-44,203,199,-43,-41,-40,-38,-36,44,0,0,0,-30,0,-1,0,46,123,0,210,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_table :: Array Int Int
alex_table = listArray (0,465) [0,30,39,9,13,20,35,12,3,4,17,15,50,10,11,18,38,43,49,0,42,47,21,25,31,48,37,52,22,28,24,27,51,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,29,6,32,1,53,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,7,33,8,14,2,2,2,2,2,2,2,2,2,2,26,23,46,0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,45,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,16,19,36,34,0,0,0,0,0,0,0,0,0,0,0,41,0,0,0,40,0,0,0,0,0,0,44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,465) [-1,33,61,62,46,37,38,46,40,41,42,43,44,45,46,47,61,61,61,-1,61,61,60,61,62,61,58,59,60,61,62,61,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,61,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,48,49,50,51,52,53,54,55,56,57,61,62,61,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,61,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,42,47,124,38,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,61,-1,-1,-1,61,-1,-1,-1,-1,-1,-1,61,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,53) [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,53) [[],[(AlexAcc (alex_action_0))],[(AlexAcc (alex_action_0))],[(AlexAcc (alex_action_1))],[(AlexAcc (alex_action_2))],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_7))],[(AlexAcc (alex_action_12))],[(AlexAcc (alex_action_8))],[(AlexAcc (alex_action_9))],[],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_11))],[(AlexAcc (alex_action_13))],[(AlexAcc (alex_action_14))],[(AlexAcc (alex_action_15))],[(AlexAcc (alex_action_16))],[(AlexAcc (alex_action_17))],[(AlexAcc (alex_action_18))],[(AlexAcc (alex_action_20))],[(AlexAcc (alex_action_19))],[(AlexAcc (alex_action_22))],[(AlexAcc (alex_action_21))],[(AlexAcc (alex_action_23))],[(AlexAcc (alex_action_24))],[(AlexAcc (alex_action_33))],[(AlexAcc (alex_action_25))],[],[(AlexAcc (alex_action_26))],[(AlexAcc (alex_action_27))],[(AlexAcc (alex_action_28))],[(AlexAcc (alex_action_29))],[(AlexAcc (alex_action_30))],[(AlexAcc (alex_action_31))],[(AlexAcc (alex_action_32))],[(AlexAcc (alex_action_34))],[(AlexAcc (alex_action_35))],[(AlexAcc (alex_action_36))],[(AlexAcc (alex_action_37))],[(AlexAcc (alex_action_38))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_40))],[(AlexAcc (alex_action_41))],[(AlexAcc (alex_action_42))],[(AlexAcc (alex_action_43))],[(AlexAcc (alex_action_44))],[(AlexAcc (alex_action_45))],[(AlexAcc (alex_action_46))],[(AlexAcc (alex_action_47))],[(AlexAcc (alex_action_48))],[(AlexAcc (alex_action_49))]]
alex_action_0 =  \loc len str -> keywordOrIdent (take len str) loc 
alex_action_1 =  openParen LeftRoundBracketToken 
alex_action_2 =  closeParen RightRoundBracketToken 
alex_action_3 =  openParen LeftSquareBracketToken 
alex_action_4 =  closeParen RightSquareBracketToken 
alex_action_5 =  openParen LeftBraceToken 
alex_action_6 =  closeParen RightBraceToken 
alex_action_7 =  symbolToken RightArrowToken 
alex_action_8 =  symbolToken DotToken 
alex_action_9 =  symbolToken EllipsisToken 
alex_action_10 =  symbolToken TildeToken 
alex_action_11 =  symbolToken PlusToken 
alex_action_12 =  symbolToken MinusToken 
alex_action_13 =  symbolToken ExponentToken 
alex_action_14 =  symbolToken MultToken 
alex_action_15 =  symbolToken DivToken 
alex_action_16 =  symbolToken FloorDivToken 
alex_action_17 =  symbolToken ModuloToken 
alex_action_18 =  symbolToken ShiftLeftToken 
alex_action_19 =  symbolToken ShiftRightToken 
alex_action_20 =  symbolToken LessThanToken 
alex_action_21 =  symbolToken LessThanEqualsToken 
alex_action_22 =  symbolToken GreaterThanToken 
alex_action_23 =  symbolToken GreaterThanEqualsToken 
alex_action_24 =  symbolToken EqualityToken 
alex_action_25 =  symbolToken NotEqualsToken 
alex_action_26 =  symbolToken NotEqualsV2Token 
alex_action_27 =  symbolToken XorToken 
alex_action_28 =  symbolToken BinaryOrToken 
alex_action_29 =  symbolToken AndToken 
alex_action_30 =  symbolToken BinaryAndToken 
alex_action_31 =  symbolToken OrToken 
alex_action_32 =  symbolToken ColonToken 
alex_action_33 =  symbolToken AssignToken 
alex_action_34 =  symbolToken PlusAssignToken 
alex_action_35 =  symbolToken MinusAssignToken 
alex_action_36 =  symbolToken MultAssignToken 
alex_action_37 =  symbolToken DivAssignToken 
alex_action_38 =  symbolToken ModAssignToken 
alex_action_39 =  symbolToken PowAssignToken 
alex_action_40 =  symbolToken BinAndAssignToken 
alex_action_41 =  symbolToken BinOrAssignToken 
alex_action_42 =  symbolToken BinXorAssignToken 
alex_action_43 =  symbolToken LeftShiftAssignToken 
alex_action_44 =  symbolToken RightShiftAssignToken 
alex_action_45 =  symbolToken FloorDivAssignToken 
alex_action_46 =  symbolToken CommaToken 
alex_action_47 =  symbolToken AtToken 
alex_action_48 =  symbolToken SemiColonToken 
alex_action_49 =  symbolToken BackQuoteToken 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 37 "templates/GenericTemplate.hs" #-}

{-# LINE 47 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 89 "templates/GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 100 "templates/GenericTemplate.hs" #-}
quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input'' len, _) ->



		AlexSkip input'' len

	(AlexLastAcc k input''' len, _) ->



		AlexToken input''' len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = check_accs (alex_accept `quickIndex` (s))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		(base) = alexIndexInt32OffAddr alex_base s
		((ord_c)) = ord c
		(offset) = (base + ord_c)
		(check)  = alexIndexInt16OffAddr alex_check offset
		
		(new_s) = if (offset >= (0)) && (check == ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	case new_s of 
	    (-1) -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (len + (1)) 
			new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (len)
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (len)
	check_accs (AlexAccPred a predx : rest)
	   | predx user orig_input (len) input
	   = AlexLastAcc a input (len)
	check_accs (AlexAccSkipPred predx : rest)
	   | predx user orig_input (len) input
	   = AlexLastSkip input (len)
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (i) = i
