import Data.Monoid

                -- Advanced Haskell --

        -- Monoids --

-- Monoids are classes and features of classes which support operation(s)
-- which are *associative* and have an *identity* element, which does not
-- change them under that operation.
--      For example:
-- Integers form a monoid under addition,       with operation `+` and identity `0`
-- Integers also form one under multiplication, with operation `*` and identity `1`
-- Lists    form a monoid under concatenation,  with operation `++` and identity `[]`

-- The Monoid class is defined in the standard Prelude as
--
-- class Monoid a where
--      mempty  :: a                    => The Identity
--      mappend :: a -> a -> a          => The associative operation
-- 
--      mconcat :: [a] -> a             => Generalised fold for monoids
--      mconcat = foldr mappend mempty

-- (<>) is also provided as a synonym for mappend
-- This is actually provided by Semigroup (the class of types which support
--                                         an associative binary operation)
-- Semigroup is a superclass of Monoid

-- The Monoid definition is extremely general, and monoids are not limited to
-- just data structures. As such, "appending" may not always be the best analogy

-- All numbers (instances of Num) form a monoid under both addition and multiplication
-- So which one to choose when `mappend`ing?
-- This is solved by creating a `newtype` for each instance:
--
-- newtype Sum a     = Sum     { getSum     :: a }
-- newtype Product a = Product { getProduct :: a }
--
-- instance Num a => Monoid (Sum a) where
--      mempty = Sum 0
--      Sum x `mappend` Sum y = Sum (x+y)
-- instance Num a => Monoid (Product a) where
--      mempty = Product 1
--      Product x `mappend` Product y = Product (x*y)

-- So 2 <> 3 doesn't actually mean anything, because mappend doesn't know what we want
-- but getSum     $ 2 <> 3 = 5
-- and getProduct $ 2 <> 3 = 6

        -- Monoid Laws
-- (x <> y) <> z = x <> (y <> z)        => Associativity
-- mempty <> x = x                      => Left-identity
-- x <> mempty                          => Right-identity

        -- Exercise: Implement at least two Monoid instances for Bool

newtype AndBool = AndBool { getAB :: Bool }
newtype OrBool  = OrBool  { getOB :: Bool }

instance Semigroup AndBool where
        AndBool a <> AndBool b = AndBool $ a && b
instance Semigroup OrBool where
        OrBool a <> OrBool b = OrBool $ a || b

instance Monoid AndBool where
        mempty = AndBool True
        mappend = (<>)
instance Monoid OrBool where
        mempty = OrBool False
        mappend = (<>)

-- These are provided by Data.Monoid as `All` and `Any` respectively

        -- Homomorphisms
-- Monoid homomorphisms are functions f :: a -> b (where a and b are monoids), such
-- that it preserves the monoid structure, like this:
-- f mempty          = mempty
-- f (x `mappend` y) = f x `mappend` f y

-- An example of this would be `length` - the homomorphism from ([], ++) to (0, +)
-- length []         = 0
-- length (xs ++ ys) = length xs + length ys

-- Homomorphisms can be useful to identify, as the implementation of one `mappend`
-- can prove more efficient than another, thus `mappend`ing should be done in that
-- monoid before being transformed into the other.
