{-# OPTIONS -fglasgow-exts -fth #-}

-- Staged Typed Compilation into GADT using typeclasses
-- Parsing and Lifting

module TermLift where

import Language.Haskell.TH hiding (Exp)
import qualified Language.Haskell.TH as TH (Exp)
import Language.Haskell.TH.Ppr

-- untyped terms, at Level 0
data Expr = ELit Int
	  | EInc Expr
	  | EIsZ Expr
	  | EPair Expr Expr
	  | EIf Expr Expr Expr
	  | EFst Expr
	  | ESnd Expr
	    deriving (Read,Show)

-- Untyped terms, at Level 1
type F = Q TH.Exp
newtype FLit      = FLit Int
newtype FInc e    = FInc e
newtype FIsZ e    = FIsZ e
data FPair e1 e2  = FPair e1 e2
data FIf e1 e2 e3 = FIf e1 e2 e3
newtype FFst e    = FFst e
newtype FSnd e    = FSnd e

parse :: Expr -> F
parse (ELit x) = [e| FLit $(litE . integerL . fromIntegral $ x) |]
parse (EInc x) = [e| FInc $(parse x) |]
parse (EIsZ x) = [e| FIsZ $(parse x) |]
parse (EFst x) = [e| FFst $(parse x) |]
parse (ESnd x) = [e| FSnd $(parse x) |]
parse (EPair x y) = [e| FPair $(parse x) $(parse y) |]
parse (EIf x y z) = [e| FIf $(parse x) $(parse y) $(parse z) |]


show_code cde = runQ cde >>= putStrLn . pprint

t0 = show_code . parse . read $ "EInc (EInc (ELit 1))"
-- TermLift.FInc (TermLift.FInc (TermLift.FLit 1))

-- e1 is an ill-typed expression
e1 = "EIf (ELit 1) (ELit 2) (ELit 3)"

e2 =         "(EIf (EIsZ (ELit 0))          " ++
             "     (EInc (ELit 1))          " ++
             "     (EFst (EPair (ELit 42)   " ++
             "                  (ELit 43))))"

t1 = show_code . parse . read $ e1
-- TermLift.FIf (TermLift.FLit 1) (TermLift.FLit 2) (TermLift.FLit 3)

