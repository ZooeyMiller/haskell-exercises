{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (TypeError, ErrorMessage(..))
-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.

{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':

type family (+) (x :: Nat) (y :: Nat) :: Nat where
  (+) 'Z     y = y
  (+) ('S x) y = 'S (x + y)

-- | b. Write a type family '**' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

-- illegal nested type family is a big yikes - undecidable instances :|
--
type family (**) (x :: Nat) (y :: Nat) :: Nat where
  (**) 'Z _     = 'Z
  (**) ('S x) y = y + (x ** y)

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | c. Write a function to add two 'SNat' values.
sAdd :: SNat a -> SNat b -> SNat (a + b) 
sAdd SZ     y = y
sAdd (SS x) y = SS (sAdd x y)


{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append VNil x = x 
append (VCons y ys) x = VCons y $ append ys x 

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
flatMap VNil _ = VNil
flatMap (VCons x xs) f = append (f x) (flatMap xs f)

-- fuckin 'ell - my implentation of flatMap (which i was certain was right)
-- was giving me the error
-- Could not deduce: (m + (n1 * m)) ~ (n1 * (m + m))
-- which made me look back at my implementation of (*) which it turned out was
-- wrong. it was of the form (y * (x + x)), and the needed form was (x + (y * x))
-- just like the type error said. Type families are good confirmed. 



{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.
type family (x :: Bool) && (y :: Bool) :: Bool where
  'True && 'True   = 'True
  'False && 'False = 'True
  _ && _           = 'False
-- | b. Write the type-level @||@ function for booleans.

type family (x :: Bool) || (y :: Bool) :: Bool where
  'True || _ = 'True
  _ || 'True = 'True
  _ || _     = 'False

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.
type family All (x :: [Bool]) :: Bool where
  All '[]          = 'True
  All ('True : xs) = All xs
  All ('False : _) = 'False




{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.
type family Compare (x :: Nat) (y :: Nat) :: Ordering where
  Compare 'Z 'Z         = 'EQ
  Compare ('S _) 'Z     = 'GT
  Compare 'Z ('S  _)    = 'LT
  Compare ('S x) ('S y) = Compare x y
-- | b. Write a 'Max' family to get the maximum of two natural numbers.

type family Max (x :: Nat) (y :: Nat) :: Nat where
  Max x y = IfGt (Compare x y) x y 

type family IfGt (o :: Ordering) (n :: Nat) (m :: Nat) :: Nat where
  IfGt 'GT n _ = n
  IfGt _ _ m   = m
-- | c. Write a family to get the maximum natural in a list.
type family MaxNat (acc :: Nat) (l :: [Nat]) :: Nat where
  MaxNat x (y:ys) = MaxNat (Max x y) ys
  MaxNat x '[]    = x




{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.

type family Insert (x :: Nat) (tree :: Tree) :: Tree where
  Insert _ Empty = Empty
  Insert x (Node t y t') = 
    Case (Compare x y) '[ 
      '( 'GT, Node t y (Insert x t'))
    , '( 'EQ, Node t y t' )
    , '( 'LT, Node (Insert x t) y t')
    ]

type family Case (x :: k) (xs :: [(k, g)]) :: g where
  Case _ '[] = TypeError ('Text "non-exhaustive Case")
  Case y ('(y, yes) ': xs) = yes
  Case y (_ ': xs) = Case y xs




{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.





{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.





{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  Every _ '[] = ()
  Every c (x ': xs) = CAppend (c x) (Every c xs)
-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?





{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

-- | b. Write a type-level prime number sieve.

-- | c. Why is this such hard work?
