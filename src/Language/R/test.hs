{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

import Data.Generics
import Data.Generics.PlateData

data Term a where
    Lit2   { val  :: Int }      :: Term Int
    Succ   { num  :: Term Int } :: Term Int
    Pred   { num  :: Term Int } :: Term Int
    IsZero { arg  :: Term Int } :: Term Bool	
    Pair   { arg1 :: Term a
           , arg2 :: Term b
           }                    :: Term (a,b)
    If     { cnd  :: Term Bool
           , tru  :: Term a
           , fls  :: Term a
           }                    :: Term a

testfunc :: Term Int -> Bool
testfunc a = True

data TType0 = TType0 deriving (Show, Data, Typeable)
data TType1 = TType1 deriving (Show, Data, Typeable) 
data TType2 = TType2 deriving (Show, Data, Typeable)
data TType3 = TTYpe3 deriving (Show, Data, Typeable)

data T a where
  T1 :: { field0 :: Integer } -> T TType1
  T2 :: { field1 :: Integer } -> T TType2

instance Show (T TType1) where
  show a@(T1 x) = show (field0 a)

instance Show (T TType2) where
  show a@(T2 x) = show (field1 a)

deriving instance Typeable1 T
-- deriving instance (Typeable1 T, Data a) => Data (T a)
-- deriving instance (Typeable1 T) => Data (T TType2)

data family TT a
data    instance TT Int  = TT1 Int | TT2 Bool
newtype instance TT Char = TTC Bool

data IntGroup = Int :+: Integer deriving (Read, Show, Eq, Ord)
instance Num IntGroup where
  (+) a b = 


ff :: IntGroup -> Bool
ff x = True

class IsShape a where
    draw :: a -> Bool

data Rectangle = Rectangle Int Int
  deriving Show

instance IsShape Rectangle where
    draw (Rectangle length width) = True

data Circle = Circle Int Int
  deriving Show

instance IsShape Circle where
    draw (Circle center radius) = True

data Shape where
    Shape :: IsShape a => a -> Shape

shapes = [Shape (Circle 5 10), Shape (Circle 20 30), Shape (Rectangle 10 15)]



data GenericPlace = forall a. Show a => GenericPlace a
-- places :: [GenericPlace]
-- places = [GenericPlace USA, GenericPlace NewYork]

data Place = Country Country
           | State State
           | City String
           deriving Show

data State = California
           | NewYork
           deriving Show

data Country = USA
             | Canada
             deriving Show


-- Type classes to categorize objects
{-
-- primitive types
data Box    = Box
data Sphere = Sphere
data Mesh   = Mesh
data Plane  = Plane

-- classes of types
type FiniteSolidObjects = Box :*: Sphere :*: HNil
type FinitePatchObjects = Mesh :*: HNil
type InfiniteSolidObjects = Plane :*: HNil

-- All of finite and infinite solid objects are solid objects
type SolidObjects = AllOf FiniteSolidObjects :*: AllOf InfiniteSolidObjects :*: HNil

-- membership predicate
-- Statically tests if an object of the type x is a member of the class c
is_of_class :: forall c x r. Apply (Member c) x r => x -> c -> r
is_of_class x t = apply (undefined::Member c) x


test1 = is_of_class Box (undefined::FiniteSolidObjects) -- type HTrue
test2 = is_of_class Box (undefined::SolidObjects) -- type HTrue
test3 = is_of_class Box (undefined::InfiniteSolidObjects) -- type HFalse
test4 = is_of_class Plane (undefined::SolidObjects) -- type HTrue

-- make a semi-predicate SolidObject

class SolidObject c
instance Apply (Member SolidObjects) x HTrue => SolidObject x

test_solid :: SolidObject x => x -> ()
test_solid = undefined

ts1 = test_solid Plane
-- ts3 = test_solid Mesh -- causes the type error

-}
data Expr = Var String
          | Lit Int
          | Call String [Expr]
            deriving (Data, Typeable, Show)

data Stmt = While Expr [Stmt]
          | Assign String Expr
          | Sequence [Stmt]
            deriving (Data,Typeable, Show)


extractLits :: Data a => a -> [Int]
extractLits = everything (++) ([] `mkQ` f)
     where f (Lit x) = [x]
           f _ = []

extractLits' :: Data a => a -> [Int]
extractLits' x = [y | Lit y <- universeBi x]


class Nameable a where 
  nameable :: a -> Bool
  nameable x = True


data NilSxpC = NilSxpC
  deriving (Read, Show, Ord, Eq, Enum, Data, Typeable)

instance Nameable NilSxpC where
  nameable x = True

data ChrSxpC = ChrSxpC
  { value_chr :: String }
  deriving Show

data SymSxpC = SymSxpC
  { name  :: ChrSxpC
  , value_sym :: String
  }


instance Nameable ChrSxpC where
  nameable x = True

data MyType1
data MyType2

type Numbery = Integer :*: Int
type MyTypes = MyType1 :*: MyType2

myfunc :: Numbery -> Bool
myfunc _ = True
  
{-
data SExp 
  = NilSxp
  | ChrSxp 
    { value :: String }
  | SymSxp
    { name        :: Either NilSxp ChrSxp
    , value       :: ChrSxp 
    , is_internal :: Bool }
  | PairListSxp
    { name :: Either NilSxp ChrSxp
    , car  :: SExp
    , cdr  :: Either PairListSxp NilSxp }
  deriving (Read, Ord, Show, Eq)
-}
