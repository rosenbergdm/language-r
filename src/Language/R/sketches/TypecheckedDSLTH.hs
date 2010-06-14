{-# OPTIONS -fglasgow-exts -fth #-}

-- Typechecker to GADT
-- Implementing a typed DSL *compiler* with the typed evaluator and the 
-- the typechecker from untyped terms to typed ones

-- We use TH later on to lift TypedTerms to true top-level typed terms.
-- That is, if we have a value (TypedTerm tp tm) of the type
-- TypedTerm defined below, where tp is of the type Typ t and tm
-- of of the type Term t for some type t, then
-- $(lift'self tm) is the same as tm -- only with the real rather than
-- existential type.


module TypecheckedDSLTH where

import Language.Haskell.TH hiding (Exp)
import Language.Haskell.TH.Syntax hiding (Exp)
import qualified Language.Haskell.TH as TH (Exp(..))
import Language.Haskell.TH.Ppr

-- Untyped terms (what I get from my parser):

data Exp =  EDouble Double | 
	    EString String | 
	    EPrim String | 
	    EApp Exp Exp deriving (Show)

-- Typed terms:

data Term a where
  Num :: Double -> Term Double
  Str :: String -> Term String
  App :: Term (a->b) -> Term a -> Term b
  -- If we don't care about TH, we should use
  -- Fun :: (a->b) -> Term (a->b)
  -- Since we don't seem to have cross-stage persistent values in TH
  -- (only global variables) and attempt to emulate causes GHC to panic,
  -- we use the safe way, and keep track of the name of the primitive
  -- function, along with its value.
  Fun :: Name -> (a->b) -> Term (a->b)


-- Typed evaluator

eval :: Term a -> a
eval (Num x) = x
eval (Str x) = x
eval (Fun _ x) = x
eval (App e1 e2) = (eval e1) (eval e2)


-- Types and type comparison

data Typ a where
    TDouble :: Typ Double
    TString :: Typ String
    TFun    :: Typ a -> Typ b -> Typ (a->b)

data EQ a b where
    Refl :: EQ a a

-- checking that two types are the same. If so, give the witness

eqt :: Typ a -> Typ b -> Maybe (EQ a b)
eqt TDouble TDouble = Just $ Refl
eqt TString TString = Just $ Refl
eqt (TFun ta1 tb1) (TFun ta2 tb2) = eqt' (eqt ta1 ta2) (eqt tb1 tb2)
 where
   eqt' :: (Maybe (EQ ta1 ta2)) -> Maybe (EQ tb1 tb2) -> 
	   Maybe (EQ (ta1 -> tb1) (ta2 -> tb2))
   eqt' (Just Refl) (Just Refl) = Just Refl
   eqt' _ _ = Nothing
eqt _ _ = Nothing

instance Show (Typ a) where
    show TDouble = "Double"
    show TString = "String"
    show (TFun ta tb) = "(" ++ show ta ++ "->" ++ show tb ++ ")"


-- Type checking
data TypedTerm = forall t. TypedTerm (Typ t) (Term t)

-- Typing environment
type Gamma = [(String,TypedTerm)]


-- Initial environment (the types of primitives)


env0 = [("rev", TypedTerm (TFun TString TString) 
	                  (Fun 'reverse reverse)),
	-- sorry, no polymorphism!
	("show", TypedTerm (TFun TDouble TString) (Fun 'show show)),
	("inc",  TypedTerm (TFun TDouble TDouble) (Fun 'inc inc)),
	("+",    TypedTerm (TFun TDouble (TFun TDouble TDouble)) 
	                   (Fun '(+) (+)))
       ]

inc x = x + 1.0

typecheck :: Gamma -> Exp -> Either String TypedTerm
  -- literals
typecheck _ (EDouble x) = Right $ TypedTerm TDouble (Num x)
typecheck _ (EString x) = Right $ TypedTerm TString (Str x)
typecheck env (EPrim x) = maybe err Right $ lookup x env
  where err = Left $ "unknown primitive " ++ x
typecheck env (EApp e1 e2) =
  case (typecheck env e1, typecheck env e2) of
    (Right e1, Right e2) -> typechecka e1 e2
    (Left err, Right _)  -> Left err
    (Right _,  Left err) -> Left err
    (Left e1,  Left e2) ->  Left (e1 ++ " and " ++ e2)


-- typecheck the application

typechecka (TypedTerm (TFun ta tb) e1) (TypedTerm t2 e2) =
  (typechecka' (eqt ta t2) tb e1 e2) :: (Either String TypedTerm)
 where
  typechecka' :: Maybe (EQ ta t2) -> Typ tb -> Term (ta->tb) -> Term t2 ->
		 Either String TypedTerm
  typechecka' (Just Refl) tb e1 e2 = Right $ TypedTerm tb (App e1 e2)
  typechecka' _ tb e1 e2 = 
     Left $ unwords ["incompatible type of the application:",
		     show (TFun ta tb), "and", show t2]

typechecka (TypedTerm t1 e1) _ = 
    Left $ "Trying to apply not-a-function: " ++ show t1



-- tests

te1 = EApp (EPrim "inc") (EDouble 10.0)
te2 = EApp (EDouble 10.0) (EPrim "inc")
te3 = EApp (EApp (EPrim "+") 
	     (EApp (EPrim "inc") (EDouble 10.0)))
           (EApp (EPrim "inc") (EDouble 20.0))
            
te4 = EApp (EPrim "rev") te3
te5 = EApp (EPrim "rev") (EApp (EPrim "show") te3)


-- typecheck-and-eval

teval :: Exp  -> String
teval exp = either (terror) (ev) (typecheck env0 exp)
 where
  terror err = "Type error: " ++ err
  ev (TypedTerm t e) = "type " ++ show t ++ ", value " ++
       (tryshow (eqt t TString) (eqt t TDouble) (eval e))
  tryshow :: Maybe (EQ t String) -> Maybe (EQ t Double) -> t -> String
  tryshow (Just Refl) _ t = t
  tryshow _ (Just Refl) t = show t
  tryshow _ _ _ = "<fun>"


-- evaluation tests

ev_te1 = teval te1
-- "type Double, value 11.0"
ev_te2 = teval te2
-- "Type error: Trying to apply not-a-function: Double"
ev_te3 = teval te3
-- "type Double, value 32.0"
ev_te4 = teval te4
-- "Type error: incompatible type of the application: (String->String) and Double"
ev_te5 = teval te5
-- "type String, value 0.23"

-- The drawback of the existential type: the following does not work
{-
testr = either (error) (ev) (typecheck env0 te3)
 where ev (TypedTerm t e) = sin (eval e)

    Inferred type is less polymorphic than expected
      Quantified type variable `t' escapes
-}


-- TH stuff
{-
we'd like a function 
   lift'self :: Term a -> Q TH.Exp 
such that
   $(lift'self term) == term
for any term :: Term a

In other words, we would like to convert any term to the code of itself.
Evaluating (i.e., `running', or `splicing') that code should 
recover the original term.

Because we deal with typed terms, the splicing should not raise any type
error.
-}

lift'self :: Term a -> ExpQ
lift'self (Num x) = [e| Num $(litE . rationalL . toRational $ x) |]
lift'self (Str x) = [e| Str $(litE . stringL $ x) |]
lift'self (Fun n _) = [e| Fun $(reifyName n) $(varE $ n) |]
lift'self (App e1 e2) = [e| App $(lift'self e1) $(lift'self e2) |]

-- Obtain the Name that corresponds to a top-level (Prelude-level)
-- Haskell identifier.
-- Given an expression such as '(+) we return the expression
-- that is the application of mkNameG_v to the correct strings.
-- That expression, when spliced in, will compute exactly the same
-- name that corresponds to the one we started with, that is, (+).
-- Note that (+) was the identifier, not the name.
-- We essentially apply the TH to itself and emulate more than one stage
-- of computation.

reifyName (Name occname (NameG VarName pn mn)) = 
			[e| mkNameG_v
			 $(litE . stringL . pkgString $ pn)
			 $(litE . stringL . modString $ mn)
			 $(litE . stringL . occString $ occname)|]


show_code cde = runQ cde >>= putStrLn . pprint


-- typecheck-and-lift. Type errors are thrown immediately

tevall :: Exp  -> ExpQ
tevall exp = either (terror) (ev) (typecheck env0 exp)
 where
  terror err = error $ "Type error: " ++ err
  ev (TypedTerm t e) = lift'self e

ts1 = show_code $ tevall te1
ts3 = show_code $ tevall te3
ts5 = show_code $ tevall te5

