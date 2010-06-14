{-# OPTIONS -fglasgow-exts #-}

-- Typechecker to GADT
-- Implementing a typed DSL with the typed evaluator and the 
-- the typechecker from untyped terms to typed ones

module TypecheckedDSL where

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
  Fun :: (a->b) -> Term (a->b)


-- Typed evaluator

eval :: Term a -> a
eval (Num x) = x
eval (Str x) = x
eval (Fun x ) = x
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

env0 = [("rev", TypedTerm (TFun TString TString) (Fun reverse)),
	-- sorry, no polymorphism!
	("show", TypedTerm (TFun TDouble TString) (Fun show)),
	("inc",  TypedTerm (TFun TDouble TDouble) (Fun (+ 1.0))),
	("+",    TypedTerm (TFun TDouble (TFun TDouble TDouble)) (Fun (+)))
       ]

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
-- Because of the new implementation of GADT pattern matching
-- in GHC 6.10, the signature is now required.
-- Kindly pointed out by Marcin Zalewski.
typechecka :: TypedTerm -> TypedTerm -> Either String TypedTerm
typechecka (TypedTerm (TFun ta tb) e1) (TypedTerm t2 e2) =
  typechecka' (eqt ta t2) tb e1 e2
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

{-
*TypecheckedDSL> teval te1
"type Double, value 11.0"

*TypecheckedDSL> teval te2
"Type error: Trying to apply not-a-function: Double"

*TypecheckedDSL> teval te3
"type Double, value 32.0"

*TypecheckedDSL> teval te4
"Type error: incompatible type of the application: (String->String) and Double"

*TypecheckedDSL> teval te5
"type String, value 0.23"
-}

