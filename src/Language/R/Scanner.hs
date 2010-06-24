{-# OPTIONS -cpp #-}
{-# LINE 1 "Scanner.x" #-}

module Language.R.Scanner where 
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Map as Map


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
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

{-# LINE 18 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  String)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,c,[]) = Nothing
alexGetChar (p,_,(c:s))  = let p' = alexMove p c in p' `seq`
                                Just (c, (p', c, s))


{-# LINE 51 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad

{-# LINE 162 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

{-# LINE 251 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper

{-# LINE 273 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

{-# LINE 297 "templates/wrappers.hs" #-}

{-# LINE 322 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.


--alexScanTokens :: String -> [token]
alexScanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'



-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

{-# LINE 354 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

alex_base :: Array Int Int
alex_base = listArray (0,77) [-8,0,109,110,0,-33,-32,-2,8,-1,2,0,75,84,105,114,123,132,0,164,241,318,395,472,549,626,703,780,857,934,1011,1088,1165,1242,1319,1396,1473,1550,0,0,0,0,0,0,0,0,0,-52,0,0,0,-49,0,0,145,0,-31,1627,1704,1781,0,0,-46,0,0,-45,0,-43,-103,0,-16,0,0,0,0,0,1858,1935]

alex_table :: Array Int Int
alex_table = listArray (0,2190) [0,8,7,8,8,7,4,4,7,-1,51,7,-1,52,55,63,64,8,66,8,8,71,69,0,8,67,2,9,75,0,70,5,39,40,48,46,38,47,57,49,8,12,12,12,12,12,12,12,12,12,72,45,54,65,62,0,73,76,76,76,76,76,24,76,76,33,76,76,76,76,29,76,76,76,76,76,20,76,76,76,76,76,76,41,0,42,50,76,74,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,43,68,44,60,-1,-1,17,-1,-1,12,12,12,12,12,12,12,12,12,13,13,13,13,13,13,13,13,13,0,1,1,0,0,0,-1,-1,15,11,0,0,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,53,0,14,0,0,0,0,0,0,0,0,-1,-1,0,0,56,61,0,0,0,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,18,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,21,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,22,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,19,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,25,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,26,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,27,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,23,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,28,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,36,77,77,77,77,77,0,0,0,0,77,0,31,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,30,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,34,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,32,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,37,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,35,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,59,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,58,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,77,0,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,2190) [-1,9,10,11,12,13,39,39,10,10,62,13,10,62,45,61,61,9,61,11,12,124,38,-1,32,33,34,35,36,-1,38,39,40,41,42,43,44,45,46,47,32,49,50,51,52,53,54,55,56,57,58,59,60,61,62,-1,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,-1,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,10,10,46,13,13,49,50,51,52,53,54,55,56,57,49,50,51,52,53,54,55,56,57,-1,34,34,-1,-1,-1,39,39,45,76,-1,-1,49,50,51,52,53,54,55,56,57,49,50,51,52,53,54,55,56,57,49,50,51,52,53,54,55,56,57,49,50,51,52,53,54,55,56,57,45,-1,69,-1,-1,-1,-1,-1,-1,-1,-1,92,92,-1,-1,60,61,-1,-1,-1,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,105,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,-1,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,77) [-1,-1,3,3,-1,6,6,-1,-1,10,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,77) [[],[(AlexAcc (alex_action_0))],[],[],[(AlexAcc (alex_action_1))],[],[],[(AlexAcc (alex_action_2))],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_9))],[(AlexAcc (alex_action_6))],[],[],[(AlexAcc (alex_action_8))],[],[(AlexAcc (alex_action_7))],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_11))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_12))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_13))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_14))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_15))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_16))],[(AlexAcc (alex_action_17))],[(AlexAcc (alex_action_18))],[(AlexAcc (alex_action_19))],[(AlexAcc (alex_action_20))],[(AlexAcc (alex_action_21))],[(AlexAcc (alex_action_22))],[(AlexAcc (alex_action_23))],[(AlexAcc (alex_action_24))],[(AlexAcc (alex_action_25))],[(AlexAcc (alex_action_26))],[(AlexAcc (alex_action_27))],[(AlexAcc (alex_action_28))],[(AlexAcc (alex_action_29))],[(AlexAcc (alex_action_30))],[(AlexAcc (alex_action_31))],[(AlexAcc (alex_action_37))],[(AlexAcc (alex_action_32))],[],[(AlexAcc (alex_action_33))],[(AlexAcc (alex_action_34))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_35))],[(AlexAcc (alex_action_38))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_40))],[(AlexAcc (alex_action_41))],[(AlexAcc (alex_action_48))],[(AlexAcc (alex_action_42))],[],[(AlexAcc (alex_action_43))],[(AlexAcc (alex_action_44))],[(AlexAcc (alex_action_45))],[(AlexAcc (alex_action_46))],[(AlexAcc (alex_action_47))],[(AlexAcc (alex_action_49))],[(AlexAcc (alex_action_50))],[(AlexAcc (alex_action_51))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_52))]]
{-# LINE 82 "Scanner.x" #-}


data IdentifierType 
  = BuiltinIdent
  | KeywordIdent
  | PrimitiveIdent
  | UserIdent
  deriving (Read, Show, Eq)



classifyIdentifier :: AlexPosn -> String -> Token
classifyIdentifier p st =
  let isBuiltin = st `elem` builtinsList
      isKeyword = st `elem` keywordList
      isOperat  = st `elem` operatorList
      idType = case (isBuiltin, isKeyword, isOperat) of
                 (_, True, _) -> KeywordIdent
                 (True, _, _) -> BuiltinIdent
                 (_, _, _   ) -> UserIdent
  in IdentifierToken p idType st



tok f p s = f p s

-- The token type:
data Token =
  DebugLinebreakToken                    |
  LinebreakToken         AlexPosn        | 
  Let                    AlexPosn        | 
  In                     AlexPosn        | 
  TrueToken              AlexPosn        | 
  FalseToken             AlexPosn        | 
  NA_Token               AlexPosn        | 
  NaN_Token              AlexPosn        | 
  NULL_Token             AlexPosn        | 
  Inf_Token              AlexPosn        | 
  CommaToken             AlexPosn        | 
  ParenLeftToken         AlexPosn        | 
  ParenRightToken        AlexPosn        | 
  BracketLeftToken       AlexPosn        | 
  BracketRightToken      AlexPosn        | 
  BraceLeftToken         AlexPosn        | 
  BraceRightToken        AlexPosn        | 
  PlusToken              AlexPosn        | 
  MinusToken             AlexPosn        | 
  MulitplyToken          AlexPosn        | 
  DivideToken            AlexPosn        | 
  PowerExponentialToken  AlexPosn        | 
  RightArrowToken        AlexPosn        | 
  DoubleRightArrowToken  AlexPosn        | 
  LeftArrowToken         AlexPosn        | 
  DoubleLeftArrowToken   AlexPosn        | 
  DotToken               AlexPosn        | 
  EllipsisToken          AlexPosn        | 
  TildeToken             AlexPosn        | 
  DivToken               AlexPosn        | 
  LessThanToken          AlexPosn        | 
  LessThanEqualsToken    AlexPosn        | 
  GreaterThanToken       AlexPosn        | 
  GreaterThanEqualsToken AlexPosn        | 
  EqualityToken          AlexPosn        | 
  NotEqualsToken         AlexPosn        | 
  BinaryOrToken          AlexPosn        | 
  AndToken               AlexPosn        | 
  BinaryAndToken         AlexPosn        | 
  OrToken                AlexPosn        | 
  ColonToken             AlexPosn        | 
  AssignToken            AlexPosn        | 
  AtToken                AlexPosn        | 
  BackQuoteToken         AlexPosn        | 
  DollarSignToken        AlexPosn        | 
  SemiToken              AlexPosn        | 
  StringToken            AlexPosn String | 
  IntegerToken           AlexPosn Int    | 
  ImaginaryToken         AlexPosn Double | 
  NumericToken           AlexPosn Double | 
  ScientificNumericToken AlexPosn String | 
  Sym                    AlexPosn Char   | 
  Var                    AlexPosn String | 
  Int                    AlexPosn Int    |
  IdentifierToken        AlexPosn IdentifierType String
  deriving (Eq,Show)



sshow :: Token -> Doc
sshow (LinebreakToken _) = black $ onwhite $ string "\n------------LINE_BREAK_HERE-------------\n"
sshow (TrueToken p)  = bold $ red $ string $ (spacePad p) ++ (show (TrueToken p))
sshow (FalseToken p) = bold $ red $ string $ (spacePad p) ++ (show (FalseToken p))
sshow (NA_Token p)   = dullcyan $ string $ (spacePad p) ++ (show (NA_Token p))
sshow (NaN_Token p)  = dullcyan $ string $ (spacePad p) ++ (show (NaN_Token p))
sshow (NULL_Token p) = dullcyan $ string $ (spacePad p) ++ (show (NULL_Token p))
sshow (Inf_Token p)  = dullcyan $ string $ (spacePad p) ++ (show (Inf_Token p))

sshow (CommaToken p)             = blue $ string $ (spacePad p) ++  (show (CommaToken p))
sshow (ParenLeftToken p)         = blue $ string $ (spacePad p) ++ (show (ParenLeftToken p))
sshow (ParenRightToken p)        = blue $ string $ (spacePad p) ++ (show (ParenRightToken p))
sshow (BracketLeftToken p)       = blue $ string $ (spacePad p) ++ (show (BracketLeftToken p))
sshow (BracketRightToken p)      = blue $ string $ (spacePad p) ++ (show (BracketRightToken p))
sshow (BraceLeftToken p)         = blue $ string $ (spacePad p) ++ (show (BraceLeftToken p))
sshow (BraceRightToken p)        = blue $ string $ (spacePad p) ++ (show (BraceRightToken p))
sshow (PlusToken p)              = magenta $ string $ (spacePad p) ++ (show (PlusToken p))
sshow (MinusToken p)             = magenta $ string $ (spacePad p) ++ (show (MinusToken p))
sshow (MulitplyToken p)          = magenta $ string $ (spacePad p) ++ (show (MulitplyToken p))
sshow (DivideToken p)            = magenta $ string $ (spacePad p) ++ (show (DivideToken p))
sshow (PowerExponentialToken p)  = magenta $ string $ (spacePad p) ++ (show (PowerExponentialToken p))
sshow (RightArrowToken p)        = magenta $ string $ (spacePad p) ++ (show (RightArrowToken p))
sshow (DoubleRightArrowToken p)  = magenta $ string $ (spacePad p) ++ (show (DoubleRightArrowToken p))
sshow (LeftArrowToken p)         = magenta $ string $ (spacePad p) ++ (show (LeftArrowToken p))
sshow (DoubleLeftArrowToken p)   = magenta $ string $ (spacePad p) ++ (show (DoubleLeftArrowToken p))
sshow (DotToken p)               = blue $ string $ (spacePad p) ++ (show (DotToken p))
sshow (EllipsisToken p)          = magenta $ string $ (spacePad p) ++ (show (EllipsisToken p))
sshow (TildeToken p)             = magenta $ string $ (spacePad p) ++ (show (TildeToken p))
sshow (DivToken p)               = magenta $ string $ (spacePad p) ++ (show (DivToken p))
sshow (LessThanToken p)          = magenta $ string $ (spacePad p) ++ (show (LessThanToken p))
sshow (LessThanEqualsToken p)    = magenta $ string $ (spacePad p) ++ (show (LessThanEqualsToken p))
sshow (GreaterThanToken p)       = magenta $ string $ (spacePad p) ++ (show (GreaterThanToken p))
sshow (GreaterThanEqualsToken p) = magenta $ string $ (spacePad p) ++ (show (GreaterThanEqualsToken p))
sshow (EqualityToken p)          = magenta $ string $ (spacePad p) ++ (show (EqualityToken p))
sshow (NotEqualsToken p)         = magenta $ string $ (spacePad p) ++ (show (NotEqualsToken p))
sshow (BinaryOrToken p)          = magenta $ string $ (spacePad p) ++ (show (BinaryOrToken p))
sshow (AndToken p)               = magenta $ string $ (spacePad p) ++ (show (AndToken p))
sshow (BinaryAndToken p)         = magenta $ string $ (spacePad p) ++ (show (BinaryAndToken p))
sshow (OrToken p)                = magenta $ string $ (spacePad p) ++ (show (OrToken p))
sshow (ColonToken p)             = blue $ string $ (spacePad p) ++ (show (ColonToken p))
sshow (AssignToken p)            = magenta $ string $ (spacePad p) ++ (show (AssignToken p))
sshow (AtToken p)                = magenta $ string $ (spacePad p) ++ (show (AtToken p))
sshow (BackQuoteToken p)         = blue $ string $ (spacePad p) ++ (show (BackQuoteToken p))
sshow (DollarSignToken p)        = blue $ string $ (spacePad p) ++ (show (DollarSignToken p))
sshow (SemiToken p)              = blue $ string $ (spacePad p) ++ (show (SemiToken p))

sshow (StringToken p s)            = bold $ green $ string $ (spacePad p) ++ ( show (StringToken p s) )
sshow (IntegerToken p s)           = bold $ cyan $ string $ (spacePad p) ++ ( show (IntegerToken p s) )
sshow (ImaginaryToken p s)         = bold $ cyan $ string $ (spacePad p) ++ ( show (ImaginaryToken p s) )
sshow (NumericToken p s)           = bold $ cyan $ string $ (spacePad p) ++ ( show (NumericToken p s) )
sshow (ScientificNumericToken p s) = bold $ cyan $ string $ (spacePad p) ++ ( show (ScientificNumericToken p s) )
sshow (Sym p s)                    = dullwhite $ string $ (spacePad p) ++ ( show (Sym p s) )
sshow (Var p s)                    = dullwhite $ string $ (spacePad p) ++ ( show (Var p s) )
sshow (Int p s)                    = dullwhite $ string $ (spacePad p) ++ ( show (Int p s) )

sshow (IdentifierToken p t s) = bold $ yellow  $ string $ (spacePad p) ++ ( show (IdentifierToken p t s) )

spacePad :: AlexPosn -> String
spacePad (AlexPn offset col row) = concat $ replicate (row-1) " "

tokpos :: AlexPosn -> (Int, Int)
tokpos (AlexPn o c r) = (c, r)


splitByLines :: [Token] -> [([Token], Int)]
splitByLines toks = 
  let row         = fst $ tokpos $ token_posn $ head toks
      (car, cdr)  = span (\z -> (fst $ tokpos $ token_posn z) == row) toks
  in (car, row):(splitByLines cdr)


-- prettyPrintPair :: Doc -> String -> IO ()
prettyPrintPair (lns, orig) = do
  putStrLn ("\n" ++ orig ++ "\n")
  putStrLn (show lns)
  return ()


-- prettyPrintTokens :: [Token] -> String -> IO ()
prettyPrintTokens toks inp = do
  let splits   = splitByLines toks
      lins     = lines inp
      outs     = map (\(tk, rw) -> (vcat (map sshow tk), lins !! (rw - 1))) splits
  -- mapM_ prettyPrintPair outs
  return outs
      

  
token_posn (StringToken p _) = p
token_posn (IdentifierToken p _ _) = p
token_posn (IntegerToken p _) = p
token_posn (ImaginaryToken p _) = p
token_posn (NumericToken p _) = p
token_posn (ScientificNumericToken p _) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p
token_posn (LinebreakToken p ) = p
token_posn (Let p ) = p
token_posn (In p ) = p
token_posn (TrueToken p ) = p
token_posn (FalseToken p ) = p
token_posn (NA_Token p ) = p
token_posn (NaN_Token p ) = p
token_posn (NULL_Token p ) = p
token_posn (Inf_Token p ) = p
token_posn (CommaToken p ) = p
token_posn (ParenLeftToken p ) = p
token_posn (ParenRightToken p ) = p
token_posn (BracketLeftToken p ) = p
token_posn (BracketRightToken p ) = p
token_posn (BraceLeftToken p ) = p
token_posn (BraceRightToken p ) = p
token_posn (PlusToken p ) = p
token_posn (MinusToken p ) = p
token_posn (MulitplyToken p ) = p
token_posn (DivideToken p ) = p
token_posn (PowerExponentialToken p ) = p
token_posn (RightArrowToken p ) = p
token_posn (DoubleRightArrowToken p ) = p
token_posn (LeftArrowToken p ) = p
token_posn (DoubleLeftArrowToken p ) = p
token_posn (DotToken p ) = p
token_posn (EllipsisToken p ) = p
token_posn (TildeToken p ) = p
token_posn (DivToken p ) = p
token_posn (LessThanToken p ) = p
token_posn (LessThanEqualsToken p ) = p
token_posn (GreaterThanToken p ) = p
token_posn (GreaterThanEqualsToken p ) = p
token_posn (EqualityToken p ) = p
token_posn (NotEqualsToken p ) = p
token_posn (BinaryOrToken p ) = p
token_posn (AndToken p ) = p
token_posn (BinaryAndToken p ) = p
token_posn (OrToken p ) = p
token_posn (ColonToken p ) = p
token_posn (AssignToken p ) = p
token_posn (AtToken p ) = p
token_posn (BackQuoteToken p ) = p
token_posn (DollarSignToken p ) = p
token_posn (SemiToken p ) = p



-- operatorList :: [String]
operatorList = [ "..." , ":" , "::" , "?" , "??" , "["
               , "[[" , "$" , "@" , "+" , "!" , "<-" 
               , "->" , "<<-" , "=" , "[<-" , "[[<-"
               , "<-" , "-" , "*" , "/" , "^" , "%%"
               , "%*%" , "<" , "<=" , "==" , "!=" , ">="
               , ">" , "|" , "||" , "&" , "&&" , "%/%" ]


keywordList :: [String]
keywordList = 
  [ "if"
  , "for"
  , "while"
  , "repeat"
  , "return"
  , "function"
  , "quote"
  , "switch"
  , "break"
  , "next"
  , "length"
  , "length<-"
  , "class"
  , "class<-"
  , "oldClass"
  , "oldCLass<-"
  , "attr"
  , "attr<-"
  , "attributes"
  , "attributes<-"
  , "names"
  , "names<-"
  , "dim"
  , "dim<-"
  , "dimnames"
  , "dimnames<-"
  , "levels<-"
  , "environment<-"
  , "storage.mode<-" ]


builtinsList :: [String]
builtinsList = 
  [ "abs"
  , "sign"
  , "sqrt"
  , "floor"
  , "ceiling"
  , "exp"
  , "expm1"
  , "log2"
  , "log10"
  , "log1p"
  , "cos"
  , "sin"
  , "tan"
  , "acos"
  , "asin"
  , "atan"
  , "cosh"
  , "sinh"
  , "tanh"
  , "acosh"
  , "asinh"
  , "atanh"
  , "gamma"
  , "lgamma"
  , "digamma"
  , "trigamma"
  , "cumsum"
  , "cumprod"
  , "cummax"
  , "cummin"
  , "Im"
  , "Re"
  , "Arg"
  , "Conj"
  , "Mod" ]

runScanner st = alexScanTokens st

{-
main = do
  s <- getContents
  print (alexScanTokens s)
-}

alex_action_0 =  tok (\p s -> StringToken p (tail $ init s) ) 
alex_action_1 =  tok (\p s -> StringToken p (tail $ init s) ) 
alex_action_2 =  tok (\p s -> LinebreakToken p) 
alex_action_5 =  tok (\p s -> IntegerToken p (read (init s) :: Int ) ) 
alex_action_6 =  tok (\p s -> ScientificNumericToken p s ) 
alex_action_7 =  tok (\p s -> ImaginaryToken p (read (init s) :: Double) ) 
alex_action_8 =  tok (\p s -> NumericToken p (read s :: Double ) ) 
alex_action_9 =  tok (\p s -> NumericToken p (fromIntegral (read s :: Int) :: Double ) ) 
alex_action_10 =  tok (\p s -> TrueToken p ) 
alex_action_11 =  tok (\p s -> FalseToken p ) 
alex_action_12 =  tok (\p s -> NA_Token p ) 
alex_action_13 =  tok (\p s -> NaN_Token p ) 
alex_action_14 =  tok (\p s -> Inf_Token p ) 
alex_action_15 =  tok (\p s -> NULL_Token p ) 
alex_action_16 =  tok (\p s -> CommaToken p ) 
alex_action_17 =  tok (\p s -> ParenLeftToken p ) 
alex_action_18 =  tok (\p s -> ParenRightToken p ) 
alex_action_19 =  tok (\p s -> BracketLeftToken p ) 
alex_action_20 =  tok (\p s -> BracketRightToken p ) 
alex_action_21 =  tok (\p s -> BraceLeftToken p ) 
alex_action_22 =  tok (\p s -> BraceRightToken p ) 
alex_action_23 =  tok (\p s -> SemiToken p ) 
alex_action_24 =  tok (\p s -> PlusToken p ) 
alex_action_25 =  tok (\p s -> MinusToken p ) 
alex_action_26 =  tok (\p s -> MulitplyToken p ) 
alex_action_27 =  tok (\p s -> DivideToken p ) 
alex_action_28 =  tok (\p s -> PowerExponentialToken p ) 
alex_action_29 =  tok (\p s -> RightArrowToken p ) 
alex_action_30 =  tok (\p s -> DoubleRightArrowToken p ) 
alex_action_31 =  tok (\p s -> LeftArrowToken p ) 
alex_action_32 =  tok (\p s -> DoubleLeftArrowToken p ) 
alex_action_33 =  tok (\p s -> DotToken p ) 
alex_action_34 =  tok (\p s -> EllipsisToken p ) 
alex_action_35 =  tok (\p s -> TildeToken p ) 
alex_action_36 =  tok (\p s -> DivToken p ) 
alex_action_37 =  tok (\p s -> LessThanToken p ) 
alex_action_38 =  tok (\p s -> LessThanEqualsToken p ) 
alex_action_39 =  tok (\p s -> GreaterThanToken p ) 
alex_action_40 =  tok (\p s -> GreaterThanEqualsToken p ) 
alex_action_41 =  tok (\p s -> EqualityToken p ) 
alex_action_42 =  tok (\p s -> NotEqualsToken p ) 
alex_action_43 =  tok (\p s -> BinaryOrToken p ) 
alex_action_44 =  tok (\p s -> AndToken p ) 
alex_action_45 =  tok (\p s -> BinaryAndToken p ) 
alex_action_46 =  tok (\p s -> OrToken p ) 
alex_action_47 =  tok (\p s -> ColonToken p ) 
alex_action_48 =  tok (\p s -> AssignToken p ) 
alex_action_49 =  tok (\p s -> AtToken p ) 
alex_action_50 =  tok (\p s -> BackQuoteToken p ) 
alex_action_51 =  tok (\p s -> DollarSignToken p ) 
alex_action_52 =  tok (\p s -> classifyIdentifier p s) 
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
