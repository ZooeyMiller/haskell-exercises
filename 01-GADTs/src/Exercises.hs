{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Exercises where





{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int      where count   = id
instance Countable [a]      where count   = length
instance Countable Bool     where count x = if x then 1 else 0


-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountableNil :: CountableList
  CountableCons :: Countable a => a -> CountableList -> CountableList

instance Show CountableList where
  show  CountableNil             = "[]"
  show (CountableCons head tail) = show head ++ " : " ++ show tail

-- | b. Write a functiodn that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList = go 0
 where
  go n CountableNil        = n
  go n (CountableCons h t) = go (n + count h) t


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero = go CountableNil
 where
  go acc CountableNil = acc
  go acc (CountableCons h t) | count h == 0 = go acc t
                             | otherwise    = go (CountableCons h acc) t


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

-- I think no? all we know is that elements of a CountableList
-- are Countable, we don't know or care about their actual type
filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"


{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  N :: AnyList
  C :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList xs = go xs N
 where
  go N             ys = ys
  go (C head tail) ys = go tail $ C head ys

-- only possible with an explicit forall a. on the passed in fn rather
-- than on filterAnyList
-- also the only existing fns of (forall a. a -> Bool) are
-- const True or const False

-- filterAnyList :: (a -> Bool) -> AnyList -> AnyList
-- filterAnyList f xs = go xs N
--  where
--   go N ys = reverseAnyList ys
--   go (C head tail) ys | f head    = go tail $ C head ys
--                       | otherwise = go tail ys

lengthAnyList :: AnyList -> Int
lengthAnyList = go 0
 where
  go n N       = n
  go n (C h t) = go (n + 1) t


foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined
-- nope

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList N = True
isEmptyAnyList _ = False

instance Show AnyList where
  show = error "What about me?"
  -- cannot guarentee showable instance for non-constrained
  -- existential type variable a so cannnot write this instance


{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- I think it's input? because it doesn't exist in the resulting type

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

-- yes and we could check if the outputs are equal

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
  fmap f (TransformWith g x) = TransformWith (f . g) x

{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

isEqual :: EqPair -> Bool
isEqual (EqPair x y) = x == y

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

-- data EqPair a where
--   EqPair :: Eq a => a -> a -> EqPair a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

-- we could write it as 
-- data EqPair a = EqPair a 
-- but we lose the constraint. 





{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox i _)) = i

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers = go 0
 where
  go :: Int -> MysteryBox a -> Int
  go n EmptyBox        = n
  go n (IntBox    _ b) = go (n + 1) b
  go n (StringBox _ b) = go (n + 1) b
  go n (BoolBox   _ b) = go (n + 1) b


-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- we can't do this because 
-- removeLayer :: MysteryBox a -> MysteryBox b
-- doesn't tell us anything about the b - we can't know the way 
-- that a maps to b




{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

hListHead :: HList (h, t) -> h
hListHead (HCons h t) = h

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

-- you can't and tom is mean

-- patternMatchMe :: HList (Int, String, Bool, ()) -> Int
-- patternMatchMe (HCons (i, _, _, _) _) = i

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- hListAppend :: HList (a, t) -> HList (a, t) -> HList (a, t)
-- hListAppend (HCons (h, t)) (HCons (h', t')) = _

{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HLeaf :: HTree Empty
  HNode :: HTree l -> c -> HTree r -> HTree (Branch l c r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (Branch l c r) -> HTree (Branch Empty c r)
deleteLeft (HNode _ c r) = HNode HLeaf c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  (==) _ _  = True

instance (Eq (HTree l), Eq c, Eq (HTree r)) => Eq (HTree (Branch l c r)) where
  (==) (HNode l c r) (HNode l' c' r') = (l == l') && (c == c') && (r == r')



{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
    ANil :: AlternatingList a b
    ACons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts = go []
 where
  go r (ACons x (ACons _ t)) = go (x : r) t
  go r (ACons x ANil       ) = x : r
  go r ANil                  = r

getSeconds :: AlternatingList a b -> [b]
getSeconds = go []
 where
  go r (ACons _ (ACons x t)) = go (x : r) t
  go r (ACons _ ANil       ) = r
  go r ANil                  = r

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues xs = (foldMap id firsts, foldMap id seconds)
 where
  firsts  = getFirsts xs
  seconds = getSeconds xs

foldValuesOneLoop :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValuesOneLoop = go (mempty, mempty)
 where
  go (x, y) (ACons x' (ACons y' t)) = go (x <> x', y <> y') t
  go (x, y) (ACons x' ANil        ) = (x <> x', y)
  go r      ANil                    = r

{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval = error "Implement me"

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

parse :: DirtyExpr -> Maybe (Expr Int)
parse = error "Implement me"

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  -- ...

-- | b. Which types are existential?

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs = error "Implement me, and then celebrate!"

