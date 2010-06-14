{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances, IncoherentInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

import Data.Data
-- import Data.Generics

data Nameable
data Unnameable
-- data SExpAny
data SExpValue
data SExpPairListable
data SExpEnvir
data SExpVector
data SExpVecOrNull

type SExpType = Int

data SExpAny = Nameable :*: Unnameable



data SExpC where
  NilSxp :: { }                              -> SExpC
  CharSxp :: { chrsxp_val  :: String }       -> SExpC -- SExpC Nameable
  SymSxp :: 
    { sxp_name        :: SExpC Nameable
    , sym_is_internal :: Bool 
    , value           :: SExpC SExpValue }   -> SExpC SExpAny
  ListSxp ::
    { tag :: SExpC a
    , cdr :: SExpC SExpPairListable
    , car :: SExpC SExpValue }               -> SExpC SExpPairListable
  CloSxp ::
    { formals :: SExpC SExpPairListable
    , body    :: SExpC SExpVector
    , env     :: SExpC SExpEnvir }           -> SExpC SExpValue
  EnvSxp ::
    { env_frame  :: SExpC SExpPairListable
    , encl_env   :: SExpC SExpEnvir
    , hash_table :: SExpC SExpVecOrNull }    -> SExpC SExpEnvir
  -- PromSxp
  -- LangSxp
  -- SpecialSxp
  -- BuiltinSxp
  LglSxp ::
    { vec_length   :: Int
    , lgl_vec_vals :: [Bool] }               -> SExpC SExpVector
  IntSxp ::
    { vec_length   :: Int
    , lgl_vec_vals :: [Bool] }               -> SExpC SExpVector
  StrSxp ::
    { vec_length   :: Int
    , lgl_vec_vals :: [Bool] }               -> SExpC SExpVector
  -- DotSxp
  RealSxp ::
    { vec_length   :: Int
    , real_vec_vals :: [Double] }            -> SExpC SExpVector
  CplxSxp ::
    { vec_length   :: Int
    , cplx_vec_vals :: [(Double, Double)] }  -> SExpC SExpVector
  -- AnySxp
  -- VecSxp
  ExprSxp ::
    { vec_length   :: Int
    , expr_vec_vals :: [SExpC SExpAny] }     -> SExpC SExpVector
  -- BCodeSxp
  -- ExtPtrSxp
  -- WeakRefSxp
  RawSxp ::
    { vec_length   :: Int
    , raw_vec_vals :: String }               -> SExpC SExpVector
  -- S4Sxp
  deriving (Typeable)

data Sy a = Nil | Node a (Table a)



instance Show (SExpC a) where
   show NilSxp = "SEXPC: Nil"
   show (CharSxp x) = "SEXPC: " ++ x

data SExp = SExp
  { node_id   :: Int
  , node_data :: SExpC Nameable
  }

