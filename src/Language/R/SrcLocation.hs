{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, CPP, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.SrcLocation
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 06/11/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------


-- module Language.R.SrcLocation where
module SrcLocation where

import Text.Parsec.Pos

import Data.Data


-- | TODO: This is primarily inspired / stolen from language-python
-- Furthermore, this is not how I want to do this, this is not where 
-- this code belongs, and this is not a fully functional implementation,
-- merely a stub.

-- | A location for a syntactic entity from the source code.
-- The location is specified by its filename, and starting row
-- and column. 

data SrcLocation = 
   Sloc { sloc_filename :: String
        , sloc_row      :: Int
        , sloc_column   :: Int 
        } 
   | NoLocation
   deriving (Eq,Ord,Show,Typeable,Data)

srcPosToLocation :: SourcePos -> SrcLocation
srcPosToLocation sp = Sloc (sourceName sp) (sourceLine sp) (sourceColumn sp)


-- | Types which have a span.
class Span a where
   getSpan :: a -> SrcSpan
   getSpan x = SpanEmpty

-- | Create a new span which encloses two spanned things.
spanning :: (Span a, Span b) => a -> b -> SrcSpan
spanning x y = combineSrcSpans (getSpan x) (getSpan y)

instance Span a => Span [a] where
   getSpan [] = SpanEmpty
   getSpan [x] = getSpan x 
   getSpan list@(x:xs) = combineSrcSpans (getSpan x) (getSpan (last list))

instance Span a => Span (Maybe a) where
   getSpan Nothing = SpanEmpty
   getSpan (Just x) = getSpan x

instance (Span a, Span b) => Span (Either a b) where
   getSpan (Left x) = getSpan x
   getSpan (Right x) = getSpan x

instance (Span a, Span b) => Span (a, b) where
   getSpan (x,y) = spanning x y

instance Span SrcSpan where
   getSpan = id


-- | Source location spanning a contiguous section of a file.
data SrcSpan
    -- | A span which starts and ends on the same line.
  = SpanCoLinear
    { span_filename     :: !String
    , span_row          :: {-# UNPACK #-} !Int
    , span_start_column :: {-# UNPACK #-} !Int
    , span_end_column   :: {-# UNPACK #-} !Int
    }
    -- | A span which starts and ends on different lines.
  | SpanMultiLine
    { span_filename     :: !String
    , span_start_row    :: {-# UNPACK #-} !Int
    , span_start_column :: {-# UNPACK #-} !Int
    , span_end_row      :: {-# UNPACK #-} !Int
    , span_end_column   :: {-# UNPACK #-} !Int
    }
    -- | A span which is actually just one point in the file.
  | SpanPoint
    { span_filename :: !String
    , span_row      :: {-# UNPACK #-} !Int
    , span_column   :: {-# UNPACK #-} !Int
    }
    -- | No span information.
  | SpanEmpty 
   deriving (Eq,Ord,Show,Typeable,Data)


instance Span SrcLocation where
   getSpan loc@(Sloc {})
      = SpanPoint 
        { span_filename = sloc_filename loc
        , span_row = sloc_row loc
        , span_column = sloc_column loc
        }
   getSpan NoLocation = SpanEmpty 

-- | Make a point span from the start of a span
spanStartPoint :: SrcSpan -> SrcSpan
spanStartPoint SpanEmpty = SpanEmpty
spanStartPoint span = 
   SpanPoint 
   { span_filename = span_filename span
   , span_row = startRow span
   , span_column = startCol span
   }

-- | Make a span from two locations. Assumption: either the
-- arguments are the same, or the left one preceeds the right one.
mkSrcSpan :: SrcLocation -> SrcLocation -> SrcSpan
mkSrcSpan NoLocation _ = SpanEmpty
mkSrcSpan _ NoLocation = SpanEmpty 
mkSrcSpan loc1 loc2
  | line1 == line2 = 
       if col2 <= col1 
          then SpanPoint file line1 col1
          else SpanCoLinear file line1 col1 col2
  | otherwise = 
       SpanMultiLine file line1 col1 line2 col2
  where
  line1 = sloc_row loc1
  line2 = sloc_row loc2
  col1 = sloc_column loc1
  col2 = sloc_column loc2
  file = sloc_filename loc1


mkSrcSpan' :: SourcePos -> SourcePos -> SrcSpan
mkSrcSpan' sps spe =
  let sl  = srcPosToLocation sps
      sl' = srcPosToLocation spe
  in mkSrcSpan sl sl'

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans SpanEmpty r = r -- this seems more useful
combineSrcSpans l SpanEmpty = l
combineSrcSpans start end
 = case row1 `compare` row2 of
     EQ -> case col1 `compare` col2 of
                EQ -> SpanPoint file row1 col1
                LT -> SpanCoLinear file row1 col1 col2
                GT -> SpanCoLinear file row1 col2 col1
     LT -> SpanMultiLine file row1 col1 row2 col2
     GT -> SpanMultiLine file row2 col2 row1 col1
  where
  row1 = startRow start
  col1 = startCol start
  row2 = endRow end
  col2 = endCol end
  file = span_filename start

-- | Get the row of the start of a span.
startRow :: SrcSpan -> Int
startRow (SpanCoLinear { span_row = row }) = row
startRow (SpanMultiLine { span_start_row = row }) = row
startRow (SpanPoint { span_row = row }) = row
startRow SpanEmpty = error "startRow called on empty span"

-- | Get the row of the end of a span.
endRow :: SrcSpan -> Int
endRow (SpanCoLinear { span_row = row }) = row
endRow (SpanMultiLine { span_end_row = row }) = row
endRow (SpanPoint { span_row = row }) = row
endRow SpanEmpty = error "endRow called on empty span"

-- | Get the column of the start of a span.
startCol :: SrcSpan -> Int
startCol (SpanCoLinear { span_start_column = col }) = col 
startCol (SpanMultiLine { span_start_column = col }) = col 
startCol (SpanPoint { span_column = col }) = col 
startCol SpanEmpty = error "startCol called on empty span"

-- | Get the column of the end of a span.
endCol :: SrcSpan -> Int
endCol (SpanCoLinear { span_end_column = col }) = col 
endCol (SpanMultiLine { span_end_column = col }) = col 
endCol (SpanPoint { span_column = col }) = col 
endCol SpanEmpty = error "endCol called on empty span"

