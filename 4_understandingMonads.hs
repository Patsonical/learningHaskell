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


                -- The Maybe Monad --

        -- Maybe is used in functions which can fail for some inputs
        -- For instance, log, sqrt, etc. are only defined for positive
        -- numbers (as far as real numbers are concerned)
        -- Maybe can let us make safe implementations of those, that
        -- don't crash when used with negative numbers

safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog n
        | n > 0     = Just (log n)
        | otherwise = Nothing

safeSqrt :: (Floating a, Ord a) => a -> Maybe a
safeSqrt n
        | n >= 0    = Just (sqrt n)
        | otherwise = Nothing

        -- Monads allow us to combine these functions easily, even
        -- though their return types are wrapped in a Maybe, and their
        -- inputs are not:

safeSqrtLog =   safeSqrt <=< safeLog
        -- Much like the unsafe, non-monadic counterpart:
unsafeSqrtLog = sqrt . log

        -- Another common use for Maybe is looking up values, for example:

scpDB = [ (173, "Peanut")
        , (682, "The Hard-to-kill reptile")
        , (049, "The Plague Doctor")
        , (096, "The Shy Guy")
        , (3008, "The Infinite IKEA")
        , (3000, "Anantashesha")
        , (500, "Panacea")
        , (001, "[REDACTED]")
        ]

        -- The Prelude actually has a lookup function, because this is so common:

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

scp173  = lookup 173 scpDB  -- Just "Peanut"
scp3008 = lookup 3008 scpDB -- Just "The Infinite IKEA"
scp049  = lookup 049 scpDB  -- Just "The Plague Doctor"
scp2521 = lookup 2521 scpDB -- Nothing

        -- This functionality can be expanded using monads to, for instance,
        -- look up something else using the value obtained from the first
        -- lookup (if Nothing is obtained, then the following lookup will
        -- return Nothing as well, thanks to the implementation of >>= in Maybe)

scpByName = [ ("Peanut", "Euclid")
            , ("The Hard-to-kill reptile", "Keter")
            , ("The Plague Doctor", "Euclid")
            , ("The Shy Guy", "Euclid")
            , ("The Infinite IKEA", "Euclid")
            , ("Anantashesha", "Thaumiel")
            , ("Panacea", "Safe")
            , ("[REDACTED]", "[REDACTED]")
            ]

getScpObjectClass :: Int -> Maybe String
getScpObjectClass n = lookup n scpDB >>= \s -> lookup s scpByName

        -- This could also be written in do-notation, like this:

-- getScpObjectClass n = do
--      s <- lookup n scpDB
--      lookup s scpByName

        -- OR

-- getScpObjectClass n = do
--      s <- lookup n scpDB
--      c <- lookup s scpByName
--      return c

        -- If any stage of the computation fails, the entire computation
        -- is guaranteed to fail (return Nothing ) thanks to the monadic
        -- nature of Maybe and its implementation of >>=

        -- Another thing Prelude provides is the `maybe` function, which
        -- allows for supplying default values in case of a Nothing output

-- maybe :: b -> (a -> b) -> Maybe a -> b
-- e.g.

objClassOf682 = maybe "We don't know" id $ getScpObjectClass 682
-- returns "Keter"
objClassOf123 = maybe "We don't know" id $ getScpObjectClass 123
-- returns "We don't know"

        -- Alternatively, Data.Maybe offers `fromMaybe`, which does the
        -- same thing, but omitting the function argument, using simply `id`


                -- The List Monad --

