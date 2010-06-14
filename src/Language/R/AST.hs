{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, CPP, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.AST
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 05/28/10
-- 
-- Description :
--    Representation of the R Abstract Syntax Tree.
-----------------------------------------------------------------------------

module Language.R.AST where

import Language.R.Token
import Language.R.SrcLocation

import Data.Data


--------------------------------------------------------------------------------

-- | Convenient access to annotations in annotated types. 
class Annotated t where
   annot :: t annot -> annot

-- | Identifier.
data Ident annot = Ident 
  { ident_string :: String
  , ident_annot  :: annot 
  } deriving (Eq, Ord, Show, Typeable, Data)

type IdentSpan = Ident SrcSpan

instance Span IdentSpan where
   getSpan = annot 

instance Annotated Ident where
   annot = ident_annot

-- | A block of statements. A suite is a group of statements controlled by a clause, 
-- for example, the body of a loop. 

-- type Suite annot = [Statement annot] 

-- type SuiteSpan = Suite SrcSpan

-- | SEXP types from `R Internals` manual section 1.1.1
data SExp = NilSxp Nilsxp
          | SymSxp Symsxp
  deriving (Read, Show, Ord, Eq)

data Nilsxp = Nilsxp
  deriving (Read, Show, Ord, Eq)
  -- -| @NULL@ object
  -- = Nilsxp

  -- | A variable name (identifier)
  -- | SymSxp
data Symsxp = Symsxp
     { sym_name     :: String
     , sym_internal :: Bool
     , sym_value    :: SExp
     }
  deriving (Read, Eq, Ord, Show)
{-
  -- | A 'pairlist' object (internal use only)
  | ListSxp
    { tag  :: Either Nilsxp SymSxp 
    , car  :: Either Nilsxp ListSxp
    , cdr  :: Either Nilsxp ListSxp
    }

  -- | Function closure
  | Cloxsp

  -- | Environment (Current frame + enclosing frame)
  | Envsxp

  -- | A 'promise' object (unevaluated-but-promised function call)
  | Promsxp

  -- | Special functions
  | Specialsxp

  -- | Builtin functions
  | Builinsxp

  -- | Internal character string
  | Charsxp

  -- | Logical vector
  | Lglsxp

  -- | Integer vector
  | Intsxp

  -- | Numeric vector
  | Realsxp

  -- | complex vector
  | Cplxsxp

  -- | Character vector
  | Strsxp

  -- | dot-dot-dot object
  | Dotsxp

  -- | The 'any' arg for generic functions 
  | Anysxp

  -- | List structure (generic vector)
  | Vexsxp

  -- | Expression vector
  | Exprsxp

  -- | Byte code
  | Bcodesxp

  -- | External pointer reference
  | Extptrsxp

  -- | Weak reference
  | Weakrefsxp

  -- | Raw vector
  | Rawsxp

  -- | S4 classes not of simple type 
  | S4sxp





data Statement annot 
   -- | While loop. 
   = While 
     { while_cond :: Expr annot  -- ^ Loop condition.
     , while_body :: Suite annot -- ^ Loop body.
     , stmt_annot :: annot
     }
   -- | For loop. 
   | For 
     { for_target :: Ident annot -- ^ Loop variables.
     , for_values :: Expr annot  -- ^ Loop generator. 
     , for_body   :: Suite annot -- ^ Loop body
     , stmt_annot :: annot
     }
   -- | Conditional statement (if-elif-else). 
   | Conditional 
     { cond_guards :: [(Expr annot, Suite annot)] -- ^ Sequence of if-elif
     , cond_else   :: Suite annot -- ^ Possibly empty unconditional else clause.
     , stmt_annot  :: annot
     }
   -- | Assignment statement. 
   | Assign 
     { assign_to   :: Ident annot -- ^ Entity to assign to. 
     , assign_expr :: Expr annot  -- ^ Expression to evaluate.
     , stmt_annot  :: annot
     }
   -- | Return statement 
   | Return 
     { return_expr :: Maybe (Expr annot)
     , stmt_annot  :: annot 
     }
   -- | Break statement 
   | Break { stmt_annot :: annot }
   -- | Continue statement 
   | Continue { stmt_annot :: annot }
   -- | Repeat statment
   | Repeat
     { repeat_expr  :: Expr annot
     , stmt_annot   :: annot
     }
   -- | Switch statement
   | Switch 
     { switch_guards :: [(Expr annot, Suite annot)]
     , stmt_annot    :: annot
     }
   -- | Expression statement. 
   | StmtExpr 
     { stmt_expr :: Expr annot
     , stmt_annot :: annot 
     }
   -- | Print statement.
   | Print 
     { print_exprs :: [Expr annot]  -- ^ Arguments to print
     , stmt_annot :: annot 
     }
   -- | Exec statement. /Version 2 only/. 
   | Exec
     { exec_expr           :: Expr annot -- ^ Expression to exec.
     , exec_globals_locals :: Maybe (Expr annot, Maybe (Expr annot))
     , stmt_annot :: annot 
     }
   -- | Function definition. 
   | Fun 
     { fun_name :: Ident annot                -- ^ Function name.
     -- , fun_args :: [Parameter annot]          -- ^ Function parameter list.
     , fun_result_annot :: Maybe (Expr annot) -- ^ Optional result annotation.
     , fun_body :: Suite annot                -- ^ Function body.
     , stmt_annot :: annot 
     }
   deriving (Eq,Ord,Show,Typeable,Data)

type StatementSpan = Statement SrcSpan

instance Span StatementSpan where
   getSpan = annot 

instance Annotated Statement where
   annot = stmt_annot 






data Expr annot
   -- | Variable.
   = Var 
     { var_ident  :: Ident annot
     , expr_annot :: annot 
     }
   -- | Literal numerical
   | Num 
     { num_value    :: Double
     , expr_literal :: String
     , expr_annot   :: annot 
     }

   -- | Literal logical
   | Bool 
     { logical_value :: Bool
     , expr_annot    :: annot 
     }

   -- | Literal \'NULL\' value.
   | Null { expr_annot :: annot } 

   -- | Literal \'NaN\' value.
   | NaN { expr_annot :: annot } 

   -- | Literal \'NA\' value.
   | NA { expr_annot :: annot } 

   -- | Literal raw
   | ByteStrings 
     { byte_string_strings :: [String]
     , expr_annot :: annot 
     }
   
   -- | Literal character
   | Char 
     { strings_strings :: String
     , expr_annot :: annot 
     }
   
   -- | Function call. 
   | Call 
     { call_fun   :: Ident annot
     -- , call_args  :: [Argument annot] -- ^ Call arguments.
     , expr_annot :: annot
     }
   deriving (Eq,Ord,Show,Typeable,Data)
 
   -- | Slicing, for example \'w [x:y:z]\'. 
   | SlicedExpr { slicee :: Expr annot, slices :: [Slice annot], expr_annot :: annot } 

   -- | Conditional expresison. 
   | CondExpr 
     { ce_true_branch :: Expr annot -- ^ Expression to evaluate if condition is True.
     , ce_condition :: Expr annot -- ^ Boolean condition.
     , ce_false_branch :: Expr annot -- ^ Expression to evaluate if condition is False.
     , expr_annot :: annot
     }

   -- | Binary operator application.
   | BinaryOp { operator :: Op annot, left_op_arg :: Expr annot, right_op_arg :: Expr annot, expr_annot :: annot }

   -- | Unary operator application.
   | UnaryOp { operator :: Op annot, op_arg :: Expr annot, expr_annot :: annot }

   -- | Anonymous function definition (lambda). 
   | Lambda { lambda_args :: [Parameter annot], lambda_body :: Expr annot, expr_annot :: annot }

   -- | Tuple. Can be empty. 
   | Tuple { tuple_exprs :: [Expr annot], expr_annot :: annot }


type ExprSpan = Expr SrcSpan

instance Span ExprSpan where
   getSpan = annot 

instance Annotated Expr where
   annot = expr_annot 

-}
