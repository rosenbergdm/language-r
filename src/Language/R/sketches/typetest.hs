{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances, IncoherentInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

data HNil = HNil
data a :*: b = a :*: b
infixr 5 :*:
data HTrue
data HFalse

data Z = Z
newtype S n = S n

class TypeCast a b | a -> b, b -> a where typeCast :: a -> b
class TypeCast' t a b | t a -> b, t b -> a where typeCast' :: t -> a -> b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t -> a -> b

instance TypeCast' () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b =>TypeCast' t a b where typeCast' = typeCast''

class TypeEq x y b | x y -> b
instance TypeEq x x HTrue
instance TypeCast HFalse b => TypeEq x y b

-- A heterogeneous apply operator

class Apply f a r | f a -> r where
  apply :: f -> a -> r
  apply = undefined
  
instance Apply (x -> y) x y where
  apply f x = f x

-- primitive types

data Box = Box
data Sphere = Sphere
data Mesh = Mesh
data Plane = Plane

-- classes of types
type FiniteSolidObject