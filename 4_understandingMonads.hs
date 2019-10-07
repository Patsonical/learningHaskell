import Data.Char (toUpper)
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

-- Lists embody nondeterminism: functions using the List monad can output
-- an arbitrary number of results (0, 1, 2, or more), just as functions
-- using the Maybe monad can output 0 or one result.

-- List return makes a list containing the one element:
-- return x = [x]
-- return :: a -> [a]   OR      return :: a -> [] a
-- just to make it obvious that this is a monad ^

-- (>>=) is more tricky
-- (>>=) :: [a] -> (a -> [b]) -> [b]
-- What it actually does is it maps the (a -> [b]) function over a and then
-- concatenates the resulting list of lists [[b]] -> [b], so
-- xs >>= f = concat (map f xs)

        -- Example: replicating bunnies
generation :: a -> [a]
generation = replicate 3

bunny = ["bunny"]
bunny1 = bunny >>= generation
-- ["bunny, "bunny, "bunny]
bunny2 = bunny >>= generation >>= generation
-- ["bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny"]
bunny3 = bunny >>= generation >>= generation >>= generation
-- You get the picture: lots of bunnies

-- This may be a silly example, but this can be used to model any process
-- that generates a series of outputs from a single input, e.g.
-- radioactive decay, chemical reactions, etc.

-- By definition, since List is a monad, it is also a functor, and applicative:

apList = (*) <$> [1,2,3] <*> [10,100,1000]
-- apList = [10, 20, 30, 100, 200, 300, 1000, 2000, 3000]
-- So <*> applies the fmapped function to every combination of elements,
-- much like a Cartesian Product

-- fs <*> xs = concatMap (\f -> map f xs) fs
-- So it turns the first list into a list of functions, and then applies
-- all of these functions to each of the second list's elements, and then
-- concatenates the whole thing (concatMap, is just concat (map f xs), exactly
-- like the list's (>>=) operator, just with different syntactic order)


                -- do Notation --

-- do Notation in Haskell is syntactic sugar for monadic operations,
-- making code more readable by translating >>= and >> (etc.) operators
-- into an imperative-like style

        -- then >>
thenEg = putStr "Hello" >>
         putStr " " >>
         putStr "World" >>
         putStrLn "!"
-- Could be rewritten as
dothenEg = do {
         putStr "Hello";
         putStr " ";
         putStr "World"; 
         putStrLn "!";
         }
-- The explicit {} and ; can be omitted for the most part

        -- bind >>=
bindEg = putStr "What is your name? " >>
         getLine >>= \x ->
         putStrLn $ "Hello " ++ x ++ "!"
-- Could be rewritten as
dobindEg = do
         putStr "What is your name? "
         x <- getLine
         putStrLn $ "Hello " ++ x ++ "!"
-- Omitted {} and ; here
-- Explicit {} and ; allows for the use of do notation on a single line
-- Simply using >> and >>= can also be used on a single line

-- In these examples, getLine has type IO String. x, on the other hand
-- has type String, which allows for things like joining with (++) and
-- printing with putStrLn, which has type String -> IO ()

        -- The fail method
-- do notation does add one thing on top of just being syntactic sugar: fail
-- do notation allows us to use pattern matching, e.g.
-- do { Just x1 <- action1 }

-- What fail allows us to do, is to explicitly raise an error on a
-- pattern-match failure. fail takes a String, and allows the programmer
-- to specify a more descriptive message than just
-- "pattern match failure somewhere"

thisWillFail :: IO ()
thisWillFail = do fail "My Error Message"

-- fail is rarely used directly, and usually it is best to rely on automatic
-- handling of pattern-match failures for a given monad

-- The last line in a do block, or in a sequence of monadic code, is what
-- the output of the function will be. If we want to do something (e.g.
-- print a line to the console as the last thing, but have the function
-- evaluate to something else, we can make the last line `return xyz`.
-- return is not like in other languages, all it does is bring xyz into
-- the monad, so that the return types match, so the function actually
-- evaluates to `return xyz`, not just xyz.
-- For example, a function terminating with a `putStrLn myString` would have
-- type IO (), whereas if we add `return myString`, it'll have type IO String

thisIsIO :: IO ()
thisIsIO = putStrLn "This is just IO ()"

thisIsIOString :: IO String
thisIsIOString = putStrLn x >> return x
        where x = "This is IO String"

ioAndUpper :: IO ()
ioAndUpper = thisIsIOString >>= putStrLn . (map toUpper)


                -- The IO Monad --


