import Control.Monad

                -- Understanding Monads --

-- Monads are Computational Units, which can be naturally chained
-- together by the use of the >>= (bind) operator:
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
        -- This takes a monadic value, and a function whose input is
        -- non-monadic, and makes that function able to use the monadic input
-- It also implements a return function, which converts a base value
-- into a monadic value, just like pure, for Applicative Functors:
        -- return :: (Monad m) => a -> m a
-- in fact, since Monad is a subclass of Applicative, the default
-- implementation of return is `return = pure`.
-- Two more functions Monad implements are >> (then) and fail.
        -- m >> n = m >>= \_ -> n
-- Basically, >> works in the same way as >>=, except it doesn't pass on a result
-- fail is a technical necessity, and doesn't really have anything to do
-- with Monads, but it handles pattern match failures in do-notation

-- Note: Since Monad is a subclass of Applicative, hence also a subclass
-- of Functor, fmap pure and (<*>) can all be used with Monads.

        -- Example: Maybe
-- return :: a -> Maybe a
-- return x = Just x
--
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- m >>= g = case m of
--      Nothing -> Nothing
--      Just x -> g x

        -- Example: Calculation VS Computation
-- let x = foo in (x+3)     ->      foo & (\x -> id (x+3))       (v & f == f v)
-- x <- foo; return (x+3)   ->      foo >>= (\x -> return (x+3))
--   ^ In a do-block
-- & combines two pure calculations: foo and id
-- >>= combines two computational steps: foo and return

        -- Common Monads:
-- Maybe        Exception/Failure
-- Error        Exception with error description
-- State        Global state
-- IO           Input/Output
-- [] (list)    Nondeterminism
-- Reader       Environment
-- Writer       Logger

        -- Note: monad composition operators:
-- >=> and <=< 
-- (the latter works like (.), the former has the arguments switched)

-- Monads, and >>= can also be defined as functors with two extra combinators:
-- fmap   :: (a -> b) -> M a -> M b
-- return :: a -> M a
-- join   :: M (M a) -> M a     -- NEW (from Control.Monad, not in Prelude)
-- Where join takes a 'container' of 'containers', and 'flattens' it
--                    ^ so many ''s because the container is just an analogy
--
-- Now, >>= can be defined as
--      m >>= g = join (fmap g m)
--
-- Likewise, join and fmap can be defined in terms of >>= and return
--      fmap f x = x >>= (return . f)
--      join x   = x >>= id

-- Since monads are also Applicative, Control.Monad also defines liftM and ap
-- Much like how `return` and `>>` are monad-only versions of `pure` and `*>`
-- respectively, `liftM` and `ap` are just `fmap` and `<*>` respectively.
-- This is mostly just historical, but is still often used for clarity,
-- using `fmap`, `pure`, `<*>`, and `*>` when dealing with (applicative)
-- functors, and using their monadic counterparts when dealing with monads


