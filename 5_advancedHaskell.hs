import Data.Monoid
import Control.Applicative

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


        -- Applicative Functors --

-- The Applicative class introduces the <*> function, which permits function
-- application in the context of the functor
-- (<*>) :: f (a -> b) -> f a -> f b

-- This allows the application of multi-parameter functions to functors, e.g.
-- (+) <$> Just 2 <*> Just 3 = 
-- Just (2+) <*> Just 3 = 
-- Just 5

-- Another function it defines is `pure`, which simply brings an arbitrary value
-- into the functorial context, e.g. (in the Maybe conetxt)
-- pure 5 = Just 5

-- Applicative Functor Laws:
-- pure id <*> v                = v                     -- Identity
-- pure f <*> pure x            = pure (f x)            -- Homomorphism
-- u <*> pure y                 = pure ($ y) <*> u      -- Interchange
-- pure (.) <*> u <*> v <*> w   = u <*> (v <*> w)       -- Composition
--      And another, which follows from the previous ones:
-- fmap f x = pure f <*> x                              -- fmap

-- pure   :: Applicative f => a -> f a
-- return :: Monad       m => a -> m a
-- These two functions are essentially identical, bar the class constraint
-- Likewise, `ap` and <*> are the same as well
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- ap    :: Monad       m => m (a -> b) -> m a -> m b

-- In fact, `return` and >>= are enough to implement `pure` and <*>:
-- pure  = return
-- (<*>) = ap
--      where ap u v = u >>= \f ->
--                     v >>= \x ->
--                     return (f x)

-- There are more monadic functions with applicative counterparts, for example:
--      Monadic      Applicative
--      (>>)         (*>)
--      liftM2       liftA2
--      mapM         traverse
--      sequence     sequenceA
--      forM_        for_


        -- Sequencing of Effects

-- What is the result of [(2*),(3*)] <*> [4,5]?
-- Is it [8,10,12,15] or [8,12,10,15]?                  -- Spoiler: it's the former
-- This ambiguity arises because there are two valid, logical ways to apply the
-- functions: 
--    you could first apply a function to all values and move on to another function
-- or you could first apply each function to a value and move on to another value
-- This difference arises from different ways of sequencing effects, and having two
-- such legal implementations indicates that [] is a non-commutative applicative functor
-- A *commutative* applicative functor leaves no such margin for ambiguity, A.K.A
--      f <$> u <*> v = flip f <$> v <*> u
-- Commutativity (or the lack thereof) affects other functions derived from (<*>) as
-- well, with (*>) being a clear example.
--      (*>) :: Applicative f => f a -> f b -> f b
-- (*>) combines effects while only preserving the value of the second argument,
-- e.g. Just 2 *> Just 3  = Just 3
--     Nothing *> Just 3  = Nothing
--      Just 2 *> Nothing = Nothing
--      etc.
-- This can be used, for instance, to check for errors in a list of values:
-- foldl (*>) (Just 0) $ [Just 4, Just 2, Nothing, Just 3] = Nothing
-- foldl (*>) (Just 0) $ [Just 4, Just 2, Just 17, Just 3] = Just 3
-- With foldr, it will return Nothing or Just 0

-- Applicative sequencing can be inverted using (<**>) and (<*)
--      NOTE: this is *not* flip (<*>) and flip (*>) as effects are always
--            sequenced from left to right
--      [(2*),(3*)] <*> [4,5]  = [8,10,12,15]
--      [4,5] <**> [(2*),(3*)] = [8,12,10,15]
-- An alternative to this is Control.Applicative.Backwards, which offers a newtype
-- for flipping the order of effects:
--      Backwards [(2*),(3*)] <*> Backwards [4,5] = Backwards [8,12,10,15]

        -- Exercises
        -- For [], implement (<*>) from scratch, as well as its incorrectly
        -- sequenced version

(<@>) :: [a -> b] -> [a] -> [b]
[]     <@> _  = []
(f:fs) <@> vl = fmap f vl ++ (fs <@> vl)
infixl 4 <@>

-- Reverse map
pam :: [a -> b] -> a -> [b]
pam []     _ = []
pam (f:fs) x = f x : pam fs x

(<|@|>) :: [a -> b] -> [a] -> [b]
_  <|@|> []     = []
fl <|@|> (x:xs) = pam fl x ++ (fl <|@|> xs)
infixl 4 <|@|>


        -- Functor, Applicative, and Monad

-- The defining functions of these three classes (omitting pure/return) are
-- fmap  :: Functor f     =>   (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- (>>=) :: Monad m       => m a -> (a -> m b) -> m b

-- Standardizing the signatures, and replacing >>= with =<< shows striking similarities
-- (<$>) :: Functor t     =>   (a ->   b) -> (t a -> t b)
-- (<*>) :: Applicative t => t (a ->   b) -> (t a -> t b)
-- (=<<) :: Monad t       =>   (a -> t b) -> (t a -> t b)

-- All of these are mapping functions over Functors. The difference is what is being
-- mapped over in each case:
-- fmap  maps arbitrary  functions over               functors
-- (<*>) maps t (a -> b) morphisms over (applicative) functors
-- (=<<) maps a -> tb    functions over (monadic)     functors

-- Using fmap, it is impossible to modify the context of the functorial value
-- Nothing stays Nothing, Just (a number) stays Just (a number), and the number
-- of items in a list does not change (for example). It can only modify the value.

-- (<*>) does not have such a restriction, and can clearly change the context:
-- [(2*), (3*)] <*> [2,5,6] = [4,10,12,6,15,18]
-- The t (a -> b) morphism carries context of its own, which is combined with the
-- context of the t a functorial value.
-- (<*>) has a more subtle restriction: while the morphisms carry context, they contain
-- ordinary (a -> b) functions, which are still unable to modify the context.
-- This means that the way the context changes is fully determined by the contexts of
-- its arguments. The values of the arguments do not affect context in any way.

-- (>>=) is a very different game. Since it maps a -> t b functions, it is able to
-- create context from values:
-- [1,2,5] >>= \x -> replicate x x = [1,2,2,5,5,5,5,5]

-- Each level offers extra flexibility, at the cost of control and guarantees about
-- the result of the computation. Performance implications might be another thing
-- to consider, as using a more powerful class can prevent useful refactorings and
-- optimizations. Therefore, one should use only as much power as is necessary.
-- Monadic code can be very powerful, however it is often worth checking whether
-- Applicative or Functor may be sufficient.

        -- Exercises
data AT a = L a | B (AT a) (AT a) deriving Show

        -- 1 --
instance Functor AT where
        fmap f (L a) = L (f a)
        fmap f (B l r) = B (fmap f l) (fmap f r)

instance Applicative AT where
        pure = L
        (L f)   <*> t = fmap f t
        (B l r) <*> t = B (l <*> t) (r <*> t)

instance Monad AT where
        return = L
        (L a)   >>= f = f a
        (B l r) >>= f = B (l >>= f) (r >>= f)

        -- 2 --
-- a --
-- Replaces each leaf with a branch containing two copies of the leaf
fructify :: AT a -> AT a
-- fructify (L x)   = B (L x) (L x)
-- fructify (B l r) = B (fructify l) (fructify r)
fructify t = t <**> B (L id) (L id)
        -- <*> and <**> essentially apply a "pattern" to items in a functor
        -- <**> needs to be used because the first argument defines the pattern
        -- so using B (L id) (L id) <*> t would make t l look like B (L _) (L _)
        -- i.e. duplicate the whole tree into two branches
        -- whereas using t <**> B (L id) (L id) will make B (L _) (L _) look like t
        -- i.e. apply B (L id) (L id) to each leaf

-- b --
-- If a branch directly contains a leaf matching the premise, replace that branch
-- with a leaf containing the provided value
prune :: a -> (a -> Bool) -> AT a -> AT a
prune z p t = case t of
        L _                 -> t
        B (L a)    (L b)    -> if p a || p b then L z else t
        B ll@(L a) rt       -> if p a then L z else B ll (prune z p rt)
        B lt       rl@(L b) -> if p b then L z else B (prune z p lt) rl
        B lt       rt       -> B (prune z p lt) (prune z p rt)
        -- Here we need to change context based on values, therefore Applicative is
        -- not sufficient. Turns out, Monad isn't sufficient either, as it would need
        -- to somehow backtrack, since >>= is unable to generate the necessary context
        -- from a branch (since B has no values of its own). Therefore we have to
        -- resort to using explicit recursion, which is even more powerful than Monad
        -- (albeit ugly and cumbersome)

-- c --
-- Make a new tree made up of two copies of the original, one modified using f
-- and the other modified using g:
--      root
--     /    \
--   f t    g t
reproduce :: (a -> b) -> (a -> b) -> AT a -> AT b
-- reproduce f g t = B (fmap f t) (fmap g t)
reproduce f g t = B (L f) (L g) <*> t
