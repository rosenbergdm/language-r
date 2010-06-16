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
-----------------------------------------------------------------------------
-- |
-- Module      : AST2
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- created     : 06/14/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

module Language.R.AST where

import Data.Typeable
import Data.Generics

import qualified Data.ByteString as B


-- {{{ Phantom types
data Nilsxp
data Symsxp
data Listsxp
data Closxp
data Envsxp
data Promsxp
data Langsxp
data Specialsxp
data Builtinsxp
data Charsxp
data Lglsxp
data Intsxp
data Realsxp
data Cplxsxp
data Strsxp
data Dotsxp
data Anysxp
data Vecsxp
data Exprsxp
data Bcodesxp
data Extptrsxp
data Weakrefsxp
data Rawsxp
data S4sxp
data Funsxp
-- }}}

-- {{{ IsSexp class and instances
class IsSexp a where
  isSexp :: a -> Bool

instance IsSexp Nilsxp where
  isSexp a = True

instance IsSexp Symsxp where 
  isSexp a = True

instance IsSexp Listsxp where 
  isSexp a = True

instance IsSexp Closxp where 
  isSexp a = True

instance IsSexp Envsxp where 
  isSexp a = True

instance IsSexp Promsxp where 
  isSexp a = True

instance IsSexp Langsxp where 
  isSexp a = True

instance IsSexp Specialsxp where 
  isSexp a = True

instance IsSexp Builtinsxp where 
  isSexp a = True

instance IsSexp Charsxp where 
  isSexp a = True

instance IsSexp Lglsxp where 
  isSexp a = True

instance IsSexp Intsxp where 
  isSexp a = True

instance IsSexp Realsxp where 
  isSexp a = True

instance IsSexp Cplxsxp where 
  isSexp a = True

instance IsSexp Strsxp where 
  isSexp a = True

instance IsSexp Dotsxp where 
  isSexp a = True

instance IsSexp Anysxp where 
  isSexp a = True

instance IsSexp Vecsxp where 
  isSexp a = True

instance IsSexp Exprsxp where 
  isSexp a = True

instance IsSexp Bcodesxp where 
  isSexp a = True

instance IsSexp Extptrsxp where 
  isSexp a = True

instance IsSexp Weakrefsxp where 
  isSexp a = True

instance IsSexp Rawsxp where 
  isSexp a = True

instance IsSexp S4sxp where 
  isSexp a = True

instance IsSexp Funsxp where 
  isSexp a = True
-- }}}

-- {{{ ASExp class and instances

data ASExp = forall a. IsSexp a => ASExp a

instance Show ASExp where
  show a = "SEXP:"

-- }}}


-- {{{ CharSxpType Class and instances

class CharSxpType a where
  toCharSxp :: a -> SExprec Charsxp

instance CharSxpType (SExprec Nilsxp) where
  toCharSxp a = CharSxp []

instance CharSxpType (SExprec Charsxp) where
  toCharSxp a = a
  
-- }}}


-- {{{ NilEnvExtPtrSxpTypeable Class and instances

class NilEnvExtPtrSxpType a where
  dummy_func :: a -> Bool


instance NilEnvExtPtrSxpType (SExprec Nilsxp) where
  dummy_func a = True

instance NilEnvExtPtrSxpType (SExprec Envsxp) where
  dummy_func a = True

instance NilEnvExtPtrSxpType (SExprec Extptrsxp) where
  dummy_func a = True


-- }}}


-- {{{ SxpAnyTypeable Class and instances

class SxpAnyType a where
  isSxp :: a -> Bool


instance SxpAnyType (SExprec Nilsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Symsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Listsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Closxp) where
  isSxp a = True

instance SxpAnyType (SExprec Envsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Charsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Promsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Langsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Specialsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Builtinsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Lglsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Intsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Realsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Cplxsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Strsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Dotsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Anysxp) where
  isSxp a = True

instance SxpAnyType (SExprec Vecsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Exprsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Bcodesxp) where
  isSxp a = True

instance SxpAnyType (SExprec Extptrsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Weakrefsxp) where
  isSxp a = True

instance SxpAnyType (SExprec Rawsxp) where
  isSxp a = True

instance SxpAnyType (SExprec S4sxp) where
  isSxp a = True


-- }}}


-- {{{ SExprecFuncType class and instances

class SxpFuncType a where
  isFunc :: a -> Bool


instance SxpFuncType (SExprec Closxp) where
  isFunc a = True

instance SxpFuncType (SExprec Langsxp) where
  isFunc a = True

instance SxpFuncType (SExprec Builtinsxp) where
  isFunc a = True


-- }}}



-- {{{ ListOrNilSxp class and instances

class ListNilSxpType a where
  toListSxp   :: a -> SExprec Listsxp


instance ListNilSxpType (SExprec Listsxp) where
  toListSxp a = a

instance ListNilSxpType (SExprec Nilsxp) where
  toListSxp a = ListSxp NilSxp NilSxp NilSxp
  
-- }}}


-- {{{ ExprType class and instances

class ExprSxpType a where
  dummy_func'   :: a -> Bool


instance ExprSxpType (SExprec Builtinsxp) where
  dummy_func' a = True

instance ExprSxpType (SExprec Langsxp) where
  dummy_func' a = True

instance ExprSxpType (SExprec Closxp) where
  dummy_func' a = True

instance ExprSxpType (SExprec Specialsxp) where
  dummy_func' a = True

  
-- }}}


{- {{{ Rinternals.h : sxpinfo_struct (== SExprecHeader here)
struct sxpinfo_struct {
    SEXPTYPE type      :  5;/* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
			     * -> warning: `type' is narrower than values
			     *              of its type
			     * when SEXPTYPE was an enum */
    unsigned int obj   :  1;
    unsigned int named :  2;
    unsigned int gp    : 16;
    unsigned int mark  :  1;
    unsigned int debug :  1;
    unsigned int trace :  1;  /* functions and memory tracing */
    unsigned int spare :  1;  /* currently unused */
    unsigned int gcgen :  1;  /* old generation number */
    unsigned int gccls :  3;  /* node class */
}; /*		    Tot: 32 */
}}} -}


-- {{{ SExprecHeader type definition

data SExprecHeader = SExprecHeader
  { sExpType       :: ASExp
  , seh_obj        :: Bool
  , seh_named      :: Int
  , seh_gp         :: Bool
  , seh_mark       :: Bool
  , seh_debug      :: Bool
  , seh_trace      :: Bool
  , seh_spare      :: Bool
  , seh_gcgen      :: Bool
  , seh_gccls      :: Int
  } deriving (Show) 

-- }}}


-- {{{ SExprec types (from RInternals.h)

#define NILSXP	     0	  /* nil = NULL */
#define SYMSXP	     1	  /* symbols */
#define LISTSXP	     2	  /* lists of dotted pairs */
#define CLOSXP	     3	  /* closures */
#define ENVSXP	     4	  /* environments */
#define CHARSXP	     9	  /* "scalar" string type (internal only)*/
#define PROMSXP	     5	  /* promises: [un]evaluated closure arguments */
#define LANGSXP	     6	  /* language constructs (special lists) */
#define SPECIALSXP   7	  /* special forms */
#define BUILTINSXP   8	  /* builtin non-special forms */
#define LGLSXP	    10	  /* logical vectors */
#define INTSXP	    13	  /* integer vectors */
#define REALSXP	    14	  /* real variables */
#define CPLXSXP	    15	  /* complex variables */
#define STRSXP	    16	  /* string vectors */
#define DOTSXP	    17	  /* dot-dot-dot object */
#define ANYSXP	    18	  /* make "any" args work.
			  --   Used in specifying types for symbol
			  --   registration to mean anything is okay  */
#define VECSXP	    19	  /* generic vectors */
#define EXPRSXP	    20	  /* expressions vectors */
#define BCODESXP    21    /* byte code */
#define EXTPTRSXP   22    /* external pointer */
#define WEAKREFSXP  23    /* weak reference */
#define RAWSXP      24    /* raw bytes */
#define S4SXP       25    /* S4, non-vector */

#define FUNSXP      99    /* Closure or Builtin or Special */
-- }}}

-- {{{ SExprec Definition 
data SExprec a where

  NilSxp :: 
    {  }                                                  -> SExprec Nilsxp

  SymSxp ::
    { sym_name    :: (CharSxpType a) => SExprec a
    , value       :: (IsSexp a)      => SExprec a
    , internal    :: Bool }                               -> SExprec Symsxp

  ListSxp :: 
    { tag         :: (CharSxpType a)    => SExprec a
    , car         :: (ListNilSxpType a) => SExprec a
    , cdr         :: (ListNilSxpType a) => SExprec a }    -> SExprec Listsxp

  CloSxp ::
    { formals     :: SExprec Listsxp 
    , body        :: SExprec Langsxp
    , clos_env    :: SExprec Envsxp  }                    -> SExprec Closxp

  EnvSxp :: 
    { frame       :: SExprec Listsxp
    , env_env     :: SExprec Envsxp
    , hash_table  :: SExprec Vecsxp  }                    -> SExprec Envsxp
  
  CharSxp ::
    { char_value  :: [Char]          }                    -> SExprec Charsxp

  PromSxp :: 
    { prom_value  :: SExprec Anysxp
    , prom_env    :: SExprec Envsxp
    , prom_expr   :: SExprec Exprsxp }                    -> SExprec Promsxp

  LangSxp ::
    { lang_tag   :: (CharSxpType a)    =>SExprec a
    , lang_car   :: (ListNilSxpType a) => SExprec a
    , lang_cdr   :: (ListNilSxpType a) => SExprec a }     -> SExprec Langsxp

  SpecialSxp ::
    {  }                                                  -> SExprec Specialsxp

  BuiltinSxp ::
    { prim_table_offset :: Int }                          -> SExprec Builtinsxp

  
  -- ??? --
  DotSxp ::
    { dot_tag     :: (CharSxpType a)    => SExprec a
    , dot_car     :: (ListNilSxpType a) => SExprec a
    , dot_cdr     :: (ListNilSxpType a) => SExprec a }    -> SExprec Dotsxp
  
  AnySxp ::
    {  }                                                  -> SExprec Anysxp

  -- Not yet implemented in R
  BCodeSxp ::
    {  }                                                  -> SExprec Bcodesxp

  ExtPtrSxp ::
    { ext_ptr_ref       :: Integer 
    , ext_protect       :: Bool
    , ext_ptr_tag       :: SExprec Symsxp }               -> SExprec Extptrsxp 

  WeakRefSxp ::
    { wkref_key       :: (NilEnvExtPtrSxpType a) => SExprec a
    , wkref_val       :: (SxpAnyType a)          => SExprec a
    , wkref_next      :: (SxpAnyType a)          => SExprec a
    , wkref_finalizer :: (SxpFuncType a)         => SExprec a }     
                                                         -> SExprec Weakrefsxp 

  RawSxp ::
    { raw_value     :: B.ByteString }                    -> SExprec Rawsxp

  S4Sxp :: 
    { s4_tag        :: SExprec Symsxp }                  -> SExprec S4sxp 

  LglSxp ::
    { logical_value :: [Bool] }                          -> SExprec Lglsxp
  
  IntSxp ::
    { int_value     :: [Int] }                           -> SExprec Intsxp
  
  RealSxp ::
    { real_value    :: [Double] }                        -> SExprec Realsxp
  
  CplxSxp ::
    { complex_value :: [(Double, Double)] }              -> SExprec Cplxsxp 
  
  StrSxp :: 
    { string_value  :: [String] }                        -> SExprec Strsxp
  
  VecSxp ::
    { list_value    :: (SxpAnyType a) => [SExprec a] }   -> SExprec Vecsxp

  ExprSxp ::
    { expr_value    :: (ExprSxpType a) => [SExprec a] }  -> SExprec Exprsxp 

  -- {{{ SExprec_vector subgroup

class SExprec_vector a where
  len     :: a -> Int
  trueLen :: a -> Int


instance SExprec_vector (SExprec Lglsxp) where
  len a = length (logical_value a)
  trueLen a = len a

instance SExprec_vector (SExprec Intsxp) where
  len a = length (int_value a)
  trueLen a = len a

instance SExprec_vector (SExprec Cplxsxp) where
  len a = length (complex_value a)
  trueLen a = len a

instance SExprec_vector (SExprec Strsxp) where
  len a = length (string_value a)
  trueLen a = len a


instance SExprec_vector (SExprec Realsxp) where
  len a = length (real_value a)
  trueLen a = len a

{-
instance SExprec_vector (SExprec Vecsxp) where
  len (VecSxp a) = length a
  trueLen a = len a

instance SExprec_vector (SExprec Exprsxp) where
  len (ExprSxp a) = length a
  trueLen a = len a
-}


data Vector_Sexprec = forall a. SExprec_vector a => Vector_Sexprec a

  -- }}}

-- }}}


{-
Nilsxp
Symsxp
Listsxp
Closxp
Envsxp
Charsxp
Promsxp
Langsxp
Specialsxp
Builtinsxp
Lglsxp
Intsxp
Realsxp
Cplxsxp
Strsxp
Dotsxp
Anysxp
Vecsxp
Exprsxp
Bcodesxp
Extptrsxp
Weakrefsxp
Rawsxp
S4sxp
Funsxp
-}
