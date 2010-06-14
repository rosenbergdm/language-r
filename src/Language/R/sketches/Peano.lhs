From oleg-at-okmij.org Thu Feb  2 22:42:08 2006
To: haskell@haskell.org
Subject: Class-parameterized classes, and the type-level logarithm
From: oleg@pobox.com
Message-ID: <20060203064208.07FBDA9D0@Adric.metnet.navy.mil>
Date: Thu,  2 Feb 2006 22:42:08 -0800 (PST)
Status: OR


We show invertible, terminating, 3-place addition, multiplication, and
exponentiation relations on type-level Peano numerals, where _any_ two
operands determine the third. We also show the invertible factorial
relation. This gives us all common arithmetic operations on Peano
numerals, including n-base discrete logarithm, n-th root, and the
inverse of factorial. The inverting method can work with any
representation of (type-level) numerals, binary or decimal.

Furthermore, the inverter itself is generic: it is a type-class
function, that is, a type-class parameterized by the type-class to
`invert'.  There has been a proposal on Haskell' to give equal rights
to types and classes.  In Haskell98+multi-parameter type classes,
classes are already first-class, for all practical purposes. We can
quite easily define (potentially, higher-order) type functions on type
classes.

Ashley Yakeley wrote:
] I know for the usual Peano representation of natural numbers, at least,
] it's possible to represent addition and subtraction with a single class
] with two fundeps (because classes represent relations between types
] rather than functions on them).

That can be done even for decimal type numerals,
cf. number-parameterized-types paper. But we can do better: we can
have _three_ functional dependencies, so that any two operands of the
type class determine the third. The key insight was the result of a
conversation with Chung-chieh Shan and Gregory Price in the evening of
Nov 10, 2003.


Although our computational algorithms, over ground numerals, are
assuredly terminating, one often needs a global analysis of
instances to see that. Hence we have to resort to the undecidable
instances extension.

> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
>
> module PeanoArithm where
>
> data Z
> data S a
> __ = __

First we define a semi-sum relation, with only two functional
dependencies:

> class Sum2 a b c | a b -> c, a c -> b
> instance Sum2 Z a a
> instance Sum2 a b c => Sum2 (S a) b (S c)

Now we add the third dependency. It is that simple.

> class Sum a b c | a b -> c, a c -> b, b c -> a
> instance (Sum2 a b c, Sum2 b a c) => Sum a b c

After defining a few numbers,

> add:: Sum a b c => a -> b -> c
> add = __
>
> type One = S Z
> type Two = S (S Z)
>
> zero = __ :: Z
> one = __ :: One
> two = __ :: Two
> three = add one two
> n4    = add two two

we see that the typechecker can indeed add and subtract:

> ta1 = \x -> (add three x `asTypeOf` three)
> ta2 = \x -> (add x two `asTypeOf` three)
> -- ta3 = \x -> (add three x `asTypeOf` two)  -- fails

	*PeanoArithm> :t ta1
	ta1 :: Z -> S (S (S Z))
	*PeanoArithm> :t ta2
	ta2 :: S Z -> S (S (S Z))

Before we move to products, we introduce a few utilities

> class NLE x y
> instance Sum2 x a y => NLE x y
>
> class Mul a b c | a b -> c  -- a single-dependency multiplication
> instance Mul Z b Z
> instance (Mul a b c', Sum c' b c) => Mul (S a) b c  -- (a+1)*b = a*b + b

The relation |NLE x y| holds if |x| is less or equal to |y|, that is,
there exists a numeral |a| that in sum with |x| gives |y|.

We now introduce the generic inverter of an arithmetic function defined
on the whole set of natural numbers. The inverter is parameterized by
the numeric function (that is, a type class) to invert. We define
the type of these classes by the following class. It maps types to
classes (see also the HList paper):

> class Registry clas a b c | clas a b -> c

The inverter, the type-class function, is so trivial that I'm ashamed 
to discuss it. It is a simple for-loop.

> class Inv clas init limit a b c | clas init a b -> c
> instance (NLE x limit, Registry clas x b a', Sum2 a' r a,  
> 	    Inv' r clas x limit a b c)
>     => Inv clas x limit a b c
> class Inv' r clas x limit a b c | r clas x a b -> c
> instance Inv' Z clas x limit a b x		-- Found it
> instance Inv clas (S x) limit a b c           -- try next x
>     => Inv' (S r) clas x limit a b c


Division is defined as an inverse of multiplication:

> data RegMul
> instance Mul a b c => Registry RegMul a b c
>
> class Div m n q | m n -> q          -- m = n * q
> instance Inv RegMul Z m m n q => Div m n q

We partially apply Inv to the right operands, and get the Div relation
in return.

> pdiv :: Div a b c => a -> b -> c
> pdiv = __
>
> -- Only where all dependencies exist...
> class Product a b c | a b -> c, a c -> b, b c -> a
> instance (Mul a b c, Div c b a, Div c a b) => Product a b c
>
> prod:: Product a b c => a -> b -> c
> prod = __

Now the typechecker can multiply and divide

> n6 = prod two three
> tm1 = \x -> (prod three x `asTypeOf` n6)
> tm2 = \x -> (prod x two `asTypeOf` n6)
> -- tm3 = \x -> (prod x n4 `asTypeOf` n6)
> tm4 = \x -> (prod x n6 `asTypeOf` n6)
> -- tm5 = \x -> (prod x zero `asTypeOf` n6)
> tm6 = \x -> (prod x zero `asTypeOf` zero)

The inferred types are

	tm1 :: S (S Z) -> S (S (S (S (S (S Z)))))
	tm2 :: S (S (S Z)) -> S (S (S (S (S (S Z)))))
	tm4 :: S Z -> S (S (S (S (S (S Z)))))

The commented-out tests raise type errors.

The exponentiation is trivial:

> class Raise b e c | b e -> c  -- c = b ^ e
> instance Raise a Z (S Z)
> instance (Raise b e c', Mul b c' c) => Raise b (S e) c

The logarithm and the n-th root are two inverses of
(partially-applied) exponentiation. It takes a couple of lines to
define each

> data RegRaiseI
> instance Raise b a c => Registry RegRaiseI a b c
>
> class Log n b e | n b -> e  -- e = log_b n
> instance Inv RegRaiseI Z n n b e => Log n b e
>
> data RegRaise
> instance Raise a b c => Registry RegRaise a b c
>
> class Root a b c | a b -> c  -- c = b-th root of a
> instance Inv RegRaise Z a a b c => Root a b c

We introduce the full exponentiation relation

> class Exp b e c | b e -> c, b c -> e, e c -> b
> instance (Raise b e c, Log c b e, Root c e b) => Exp b e c
>
> pexp:: Exp a b c => a -> b -> c
> pexp = __

and test it

> n8 = add n4 n4
> n9 = prod three three
> te1 = pexp three two
> te2 = \x -> (pexp two x `asTypeOf` (add n8 n8))
> te3 = \x -> (pexp x three `asTypeOf` n8)
> -- te4 = \x -> (pexp x three `asTypeOf` n6)
> te5 = \x -> (pexp three x `asTypeOf` three)

*PeanoArithm> :t te2
te2 :: S (S (S (S Z)))
       -> S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))
*PeanoArithm> :t te3
te3 :: S (S Z) -> S (S (S (S (S (S (S (S Z)))))))

Indeed, 4 is the binary logarithm of 16, and two is the cubic root of 8.

Having gone that far, it is impossible to avoid factorial and its
inverse (as an evidence of further evolution of the Haskell programmer)

> class FactF a b | a -> b
> instance FactF Z (S Z)
> instance (FactF a b', Mul (S a) b' b) => FactF (S a) b
>
> data RegFact
> instance FactF a c => Registry RegFact a b c
>
> class FactR a c | a -> c    -- Inverse of Fact
> instance Inv RegFact (S Z) a a Z c => FactR a c
>
> class Fact a b | a -> b, b -> a
> instance (FactF a b, FactR b a) => Fact a b
>
> fact :: Fact a b => a -> b
> fact = __

-- :t fact n4

*PeanoArithm> :t \x -> (fact x  `asTypeOf` (prod n4 n6))
\x -> (fact x  `asTypeOf` (prod n4 n6)) :: S (S (S (S Z)))
	   -> S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))))))))

Indeed, 4 is the inverse factorial of 24. It took only a split second.


The practical outcome of this exercise is the creation of a benchmark
suite for Haskell typecheckers, e.g., computing the inverse factorial
of 720. Perhaps there should be a shoot-out entry for the speed of
typechecking.

The implementation of RSA on type level is left for future work.


> -- This is for printing only
> class Nat a where nat :: Num b => a -> b
> instance Nat Z where nat _ = 0
> instance Nat a => Nat (S a) where nat _ = 1 + (nat (__::a))

