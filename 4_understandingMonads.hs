import Data.Char (toUpper, toLower, isAlpha, isNumber, isPunctuation)
import Control.Monad
import Control.Monad.Trans.State
import Control.Applicative
import Data.Foldable (asum)
import Data.Function ((&))
import System.Random

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

-- Actions =/= Functions
-- The IO type constructor provides a way to represent actions as Haskell values
-- These actions can then be manipulated with pure functions

        -- Monadic Control Structures
-- How to repeat IO (or any other monadic) actions?

fiveGetLinesWrong = replicate 5 getLine
-- This will not work, because this creates a list of IO actions, not
-- an IO action that returns a string, i.e. [IO String] rather than IO [String]
-- What we need is to fold this list of IO actions, execute each one, and then
-- put all the *results* into a list. For that we have `sequence`

-- sequence :: (Monad m) => [m a] -> m [a]
-- So the desired function would look like this:

fiveGetLines = sequence $ replicate 5 getLine

-- Since replicate and sequence make such a perfect combination, Control.Monad
-- provides a function that combines the two: `replicateM`

fiveGetLinesAlt = replicateM 5 getLine

-- Another useful monadic version of a function is `mapM`, which joins `map` and
-- `sequence`, which makes actions from a list of values, and collects results

-- mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
-- e.g.

fToMap :: String -> IO String
fToMap x = (putStrLn . map toUpper) x >> (return . map toLower) x

putUpperReturnLower :: [String] -> IO [String]
putUpperReturnLower = mapM fToMap

tryThis :: IO [String]
tryThis = putUpperReturnLower ["This", "List", "Of", "Strings"]

-- These functions also have special "underscored" versions: `sequence_`,
-- `replicateM_` and `mapM_`. These basically just discard the final value,
-- which is good for simply sequencing actions, without collecting values.
-- (works just like >> does for >>=)
-- There is also `forM` and `forM_`, which are argument-flipped
-- versions of `mapM` and `mapM_`, respectively
-- forM :: (Monad m) => [a] -> (a -> m b) -> m [b]

printList :: (Show a) => [a] -> IO ()
printList = mapM_ (putStrLn . show)


                -- The State Monad --

-- In Haskell, global state is tricky, because functions are meant to be pure
-- The solution to this is to pass the state explicitely between function calls
-- Case Study: (Pseudo) Random Numbers

-- System.Random provides a random number generator, that can supply random
-- numbers by storing the state outside the program: `randomIO` and `randomRIO`

-- randomIO :: Random a => IO a
-- randomRIO :: Random a => (a,a) -> IO a

-- e.g. rolling two dice

rollDiceIO :: IO (Int, Int)
rollDiceIO = (,) <$> (randomRIO (1,6)) <*> (randomRIO (1,6))
-- or      = liftA2 (,) (randomRIO (1,6)) (randomRIO (1,6))
-- where     liftA2 f u v = f <$> u <*> v

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = replicateM n (randomRIO (1,6))

        -- Getting rid of IO

generator :: StdGen
generator = mkStdGen 0          -- 0 is the seed
-- generator
-- 1 1

-- random :: (RandomGen g, Random a) => g -> (a, g)

firstRandom = random generator :: (Int, StdGen)
-- We could change the Int in the type annotation to anything though

-- Now rolling two dice without IO
rollDiceClumsy :: (Int, Int)
rollDiceClumsy = (n, m)
        where   (n, g) = randomR (1,6) (mkStdGen 0)
                (m, _) = randomR (1,6) g

-- And here, with the ability to pass in a generator, instead of using mkStdGen 0
rollDiceClumsy' :: StdGen -> ((Int, Int), StdGen)
rollDiceClumsy' g = ((n, m), g2)
        where   (n, g1) = randomR (1,6) g
                (m, g2) = randomR (1,6) g1

randomRoll1 = rollDiceClumsy' generator
randomRoll2 = rollDiceClumsy' $ snd randomRoll1
randomRoll3 = rollDiceClumsy' $ snd randomRoll2

-- This is clumsy, because we manually pass the generator state
-- What we need is an automatic way to extract this second value - the new
-- generator - and pass it to the next function. That is where State comes in.

        -- This is also where it gets weird...

-- newtype State s a = State { runState :: s -> (a, s) }
-- This `State` data type doesn't really contain a state per se,
-- what it contains is a *state processor*
-- runState is the *accessor*, which eliminates the need for pattern-matching
-- to access the wrapped state processor

-- `newtype` is used instead of `data` when the type has only one constructor
-- and only one field (as is the case with State). This makes it easier for
-- the compiler to trivialise wrapping and unwrapping the single field.

-- This "state processor" stuff is a little confusing, maybe this will help:
--      "Normal" passing of state:
--      (a,s) -> (b,s)
--      Another way:
--      a -> s -> (b,s)
--      or
--      a -> (s -> (b,s))

-- state :: (s -> (a, s)) -> State s a
-- State gets weirder - it doesn't explicitely use the `State` type constructor
-- instead, the Control.Monad.Trans.State (transformers) package implements
-- the state type in a different way, albeit that does not change the way it
-- is used or understood, with the exception that instead of using the `State`
-- constructor directly, we use the function `state` to make our state processor
-- This weirdness will be addressed a few chapters down, don't worry.

        -- For every type `s`, State s can be made a Monad instance
        --      *not* State itself, *State s*
        -- where s can be any type - String, Char, Int, what have you
        -- This is because State takes *two* type parameters, not one
        -- Therefore State itself cannot be made an instance of Monad
        -- However, we need only one general definition to cover them all

-- instance Functor (State s) where
--      fmap  = Control.Monad.liftM
-- 
-- instance Applicative (State s) where
--      pure  = return
--      (<*>) = Control.Monad.ap
-- 
-- instance Monad (State s) where
--      return :: a -> State s a
--      return x = state (\s -> (x, s))
--
--      (>>=) :: State s a -> (a -> State s b) -> State s b
--      p >>= k = State $ \ s0 ->
--              let (x, s1) = runState p s0
--              in runState (k x) s1

        -- Setting and accessing state
-- put newState = State $ \_ -> ((), newState)
--      this creates a new state processor from a value, that ignores its input
-- get = State $ \s (s, s)
--      this returns the unchanged state as both the value and the next state
--      allowing it to be manipulated, and used in subsequent calls
-- evalState :: State s a -> s -> a
-- evalState p s = fst (runState p s)
--      this will just give back the new value of the state processor
-- execState :: State s a -> s -> s
-- execState p s = snd (runState p s)
--      this will just give back the new state

        -- Now applying this to the dice roll
-- The type of the state processors will be State StdGen Int
--                                  A.K.A. (StdGen -> (Int, StdGen))

-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
-- In our case, the type is (Int, Int) -> StdGen -> (Int, StdGen)
--                       or (Int, Int) -> State StdGen Int
--                                        (minus the wrapping/unwrapping)

rollDie :: State StdGen Int
rollDie = state $ randomR (1,6)

rollDieVerbose :: State StdGen Int
rollDieVerbose = do generator <- get
                    let (val, newGen) = randomR (1,6) generator
                    put newGen
                    return val

rollDieVerbose' :: State StdGen Int
rollDieVerbose' = get >>= \ generator ->
                  let (val, newGen) = randomR (1,6) generator
                  in put newGen >> return val

-- e.g. evalState rollDie (mkStdGen 0) = 6
-- same as  randomR (1,6) (mkStdGen 0) = 6

rollDice = (,) <$> rollDie <*> rollDie
-- evalState rollDice (mkStdGen 7) = (6,1)

rollNDice :: Int -> State StdGen [Int]
rollNDice n = replicateM n rollDie
-- evalState (rollNDice 4)  (mkStdGen 0) = [6,6,4,1]
-- evalState (rollNDice 7)  (mkStdGen 0) = [6,6,4,1,5,2,4]
-- evalState (rollNDice 12) (mkStdGen 5) = [6,2,2,1,3,2,5,1,5,4,2,2]


                -- Alternative and MonadPlus --

-- Alternative (from Control.Applicative), is a subclass of Applicative
-- used to combine the effects of two computations into a single one.
-- It defines two functions:
-- class Applicative f => Alternative f where
--      empty :: f a               : applicative computation with zero results
--      (<|>) :: f a -> f a -> f a : combines two applicative computations

        -- Example Instances:
-- Maybe
-- instance Alternative Maybe where
--      empty               = Nothing
--      Nothing <|> Nothing = Nothing : Zero results
--      Just x  <|> Nothing = Just x  : One result
--      Nothing <|> Just y  = Just y  : One result
--      Just x  <|> Just y  = Just x  : Still one result (discards the second)
--                                      since Maybe can only hold one value

-- List
-- instance Alternative [] where
--      empty = []
--      (<|>) = (++)

        -- Example Use: Parsing

-- Checks if a string starts with a certain digit
digit :: Int -> String -> Maybe Int
digit _ []    = Nothing
digit i (c:_) = if i > 9 || i < 0 then Nothing else
                if [c] == show i  then Just i  else Nothing

-- Checks if the digit at the start is a 0 or a 1 (runs two parsers in parallel)
binChar :: String -> Maybe Int
binChar s = digit 0 s <|> digit 1 s


-- MonadPlus is to monads what Alternative is to applicative functors
-- class Monad m => MonadPlus m where
--      mzero :: m a
--      mplus :: m a -> m a -> m a

-- For types which are instances of both Alternative and MonadPlus,
-- mzero and mplus should be equivalent to empty and <|> respectively

-- asum / msum: These fold lists of Alternative or MonadPlus values using <|>

-- Here is an example of checking a list of strings whether they
-- start with 1, 2, or 3
char123 :: String -> Maybe Int
char123 s = asum $ digit <$> [1,2,3] <*> [s]
each123 :: [String] -> [Maybe Int]
each123 = fmap char123

-- And here it is, generalised to any number of starting Ints
charNum :: [Int] -> String -> Maybe Int
charNum ints s = asum $ digit <$> ints <*> [s]
eachNum :: [Int] -> [String] -> [Maybe Int]
eachNum = fmap . charNum
--             ^ this is needed, because fmap expects a function with one argument
--               and charNum has two: [Int] and String
--               each123 doesn't need this because char123 only expects String

        -- Now
        -- each123 = eachNum [1,2,3] = fmap (charNum [1,2,3])
        --                      (notice how this doesn't need .)


                -- Monad Transformers --

-- Monad Transformers allow us to combine two monads into a
-- single one that shares the behaviour of both

-- e.g. IO + Maybe: Password Validation

isValid :: String -> Bool
isValid s = length s >= 8
         && any isAlpha s
         && any isNumber s
         && any isPunctuation s

        -- without monad transformers:

getPassNT :: IO (Maybe String)
getPassNT = getLine >>= \s ->
            if isValid s
            then return $ Just s
            else return Nothing
        -- Notice: Maybe isn't actually being used as a monad, this is a function
        --         in the IO monad that happens to return a Maybe value

askPassNT :: IO ()
askPassNT = putStr "Enter password: " >>
            getPassNT >>= \maybe_pass ->
            case maybe_pass of
                 Just pass -> do putStrLn "Password secure."
                 Nothing   -> do putStrLn "Password NOT secure."
        -- And a follow-up function still has to do pattern matching to check
        -- the Maybe, which is exactly what monads are supposed to save us from


        -- WITH monad transformers

-- MaybeT is a wrapper around m (Maybe a), where m can be any monad (e.g. IO)
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
        return = MaybeT . return . Just
        --     = MaybeT . return . return
        --    constructor    m     Maybe
        
        -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
        x >>= f = MaybeT $ runMaybeT x >>= \maybeVal ->
                           case maybeVal of
                                Nothing  -> return Nothing
                                Just val -> runMaybeT $ f val

instance Monad m => Applicative (MaybeT m) where
        pure = return
        (<*>) = ap

instance Monad m => Functor (MaybeT m) where
        fmap = liftM
