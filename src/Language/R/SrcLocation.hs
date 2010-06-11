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


module Language.R.SrcLocation where


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

-- | Types which have a span.
class Span a where
   getSpan :: a -> SrcSpan
   getSpan x = SpanEmpty

-- | Create a new span which encloses two spanned things.
spanning :: (Span a, Span b) => a -> b -> SrcSpan
spanning x y = combineSrcSpans (getSpan x) (getSpan y)

combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans a b = a

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



