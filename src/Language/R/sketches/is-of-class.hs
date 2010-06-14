{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

-- Type classes to categorize objects

module Membership where

-- primitive types
data Box    = Box deriving Show
data Sphere = Sphere deriving Show
data Mesh   = Mesh  deriving Show
data Plane  = Plane  deriving Show

-- classes of types
type FiniteSolidObjects = Box :*: Sphere :*: HNil
type FinitePatchObjects = Mesh :*: HNil
type InfiniteSolidObjects = Plane :*: HNil

-- All of finite and infinite solid objects are solid objects
type SolidObjects = AllOf FiniteSolidObjects :*: AllOf InfiniteSolidObjects
    :*: HNil

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



-- The following is borrowed verbatim from poly2.hs

type Fractionals = Float :*: Double :*: HNil
type Nums = Int :*: Integer :*: AllOf Fractionals :*: HNil
type Ords = Bool :*: Char :*: AllOf Nums :*: HNil
type Eqs  = AllOf (TypeCl OpenEqs) :*: AllOfBut Ords Fractionals :*: HNil


-- The Fractionals, Nums and Ords above are closed. But Eqs is open
-- (i.e., extensible), due to the following:
data OpenEqs
instance TypeCls OpenEqs () HTrue -- others can be added in the future

-- Type class membership testing

data AllOf x
data AllOfBut x y
data TypeCl x

-- Classifies if the type x belongs to the open class labeled l
-- The result r is either HTrue or HFalse
class TypeCls l x r | l x -> r

-- the default instance: x does not belong
instance TypeCast r HFalse => TypeCls l x r

-- Deciding the membership in a closed class, specified
-- by enumeration, union and difference

data Member tl
instance Apply (Member HNil) x HFalse

instance TypeCls l x r => Apply (Member (TypeCl l)) x r

instance (TypeEq h x bf, MemApp bf t x r) 
    => Apply (Member (h :*: t)) x r

instance (Apply (Member h) x bf, MemApp bf t x r)
    => Apply (Member ((AllOf h) :*: t)) x r

instance (Apply (Member exc) x bf, Apply (MemCase2 h t x) bf r)
    => Apply (Member ((AllOfBut h exc) :*: t)) x r

class MemApp bf t x r | bf t x -> r
instance MemApp HTrue t x HTrue
instance Apply (Member t) x r => MemApp HFalse t x r

-- we avoid defining a new class like MemApp above.
-- I guess, after Apply, we don't need a single class ever?
data MemCase2 h t x
instance Apply (Member t) x r => Apply (MemCase2 h t x) HTrue r
instance Apply (Member ((AllOf h) :*: t)) x r 
    => Apply (MemCase2 h t x) HFalse r

testm1 = apply (undefined::Member Fractionals) (1::Float)
testm2 = apply (undefined::Member Fractionals) (1::Int)
testm3 = apply (undefined::Member Fractionals) ()


-- The standard HList stuff, extracted from HList library


data HNil = HNil deriving Show
data a :*: b = a :*: b
infixr 5 :*:
data HTrue 
data HFalse

instance Show HTrue where
  show a = "--HTRUE--"

instance Show HFalse where
  show a = "--HFALSE--"

data Z = Z
newtype S n = S n


class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

class  TypeEq x y b | x y -> b
instance TypeEq x x HTrue
instance TypeCast HFalse b => TypeEq x y b


-- A heterogeneous apply operator

class Apply f a r | f a -> r where
  apply :: f -> a -> r
  apply = undefined

-- Normal function application
instance Apply (x -> y) x y where
  apply f x = f x


-- my stuff starts here

data TestA1 = TestA1
  { testField :: TypeCl Fractionals }

