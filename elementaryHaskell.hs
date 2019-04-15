                -- Lists II (map) --

-- Specific:
doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (x:xs) = (2 * x) : doubleList xs

-- More general:
multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (x:xs) = (m * x) : multiplyList m xs
        
        {- Now, doubleList may be rewritten as:
         - doubleList = multiplyList 2
         -}

-- Even more general:
applyToIntegers :: (Integer -> Integer) -> [Integer] -> [Integer]
applyToIntegers _ [] = []
applyToIntegers f (x:xs) = (f x) : applyToIntegers f xs
        --      ^ yes, this function takes another FUNCTION as an argument
        
{- And now, multiplyList may be rewritten as:
 - multiplyList = applyToIntegers ((*) m)
 - And doubleList as:
 - doubleList = applyToIntegers ((*) 2)
 -}

{-      Interlude: Currying
 - If all this abstraction confuses you, consider a concrete example:
 - When we multiply 5 * 7 in Haskell, the (*) function doesn't just take
 - two arguments at once, it actually first takes the 5 and returns a 
 - new `5*` function; and that new function then takes a second argument
 - and multiplies that by 5. So, for our example, we then give the 7 as
 - an argument to the `5*` function, and that returns us our final evaluated
 - number (35).
 - 
 - So, *all functions in Haskell really take only one argument*.
 - However, when we know how many intermediate functions we will generate
 - to reach a final result, we can treat functions as if they take many
 - arguments. The number of arguments we generally talk about functions
 - taking is actually the number of one-argument functions we get between
 - the first argument and a final, non-functional result value.
 -
 - The process of creating intermediate functions when feeding arguments
 - into a complex function is called currying (named after Haskell Curry,
 - also the namesake of the Haskell programming language).
 -}

-- EVEN MORE general:
-- Make this polymorphic (so it doesn't have to only apply to Integers)
-- Well, actually, Prelude already *has* a function for that - `map`:

{- map :: (a -> b) -> [a] -> [b]
 - map _ [] = []
 - map f (x:xs) = (f x) : map f xs
 -}

        -- e.g.
doubleThis x = 2*x              -- Simple function
list0 = [1,2,3,4]               -- List to operate on
list1 = map doubleThis list0    -- Evaluates to [2,4,6,8]
list2 = map (\x -> 3*x) list0   -- Evaluates to [3,6,9,12] (lambda expression)

        -- Note that `(map . doubleThis) list0` would be incorrect
        -- as `doubleThis` is actually an *argument* passed to `map`

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n-1) xs



                -- Lists III (folds, comprehentions) --

-- A fold is a function that takes a data structure and compresses it
-- into a single value (or other data structure).
-- e.g. `sum`, `product`, `concat`

-- Prelude implements two fold functions by default - foldr and foldl.
-- There are also *strict* versions of these: foldr' and foldl'
-- which force evaluations (instead of being lazy).
-- Prelude also implements foldr1 and foldl1, which don't use an explicit
-- accumulator, instead simply using the last element of the list.

-- Another function implemented in Prelude is `filter`, which takes in
-- a function returning a boolean (a condition) and a list, and returns
-- a list of items matching the condition.

-- Comprehensions are then just syntactic sugar for maps and filters
        -- This stuff: [x^2 | x <- [0..9], x `mod` 2 == 0]

returnDivisible :: Int -> [Int] -> [Int]
returnDivisible 0 _ = error "Cannot divide by zero"
returnDivisible x l = [n | n <- l, n `mod` x == 0]

choosingTails :: [[Int]] -> [[Int]]
choosingTails l = [tail li | li <- l, li /= [], head li > 5]

compMap :: (a -> b) -> [a] -> [b]
compMap f l = [f x | x <- l]

compFilter :: (a -> Bool) -> [a] -> [a]
compFilter f l = [x | x <- l, f x]

doubleOfFirstForEvenSeconds :: [(Int, Int)] -> [Int]
doubleOfFirstForEvenSeconds ps = map (\x -> 2*(fst x)) $ filter (\x -> snd x `mod` 2 == 0) ps



                -- Type Declarations --

-- `data` initialises a new data type
-- `type` creates an alias/alternative name for an existing type

data User = Giggoer String String [String] Bool Bool            -- name email bookmarks pretty_mode dark_mode 
          | Venue   String String (Int, Int) Int Bool Bool      -- name email location capacity pretty_mode dark_mode 

-- Declare a new data type User, which can be either a Giggoer or a Venue (WAD2 ftw).
-- A Giggoer *contains* two strings, a string list and two booleans
-- A Venue *contains* two strings, an (Int, Int) tuple, an Int and two booleans

-- The | separates the possibilities, and the Giggoer... and Venue... are constructor
-- functions for the User type.
-- Constructors are normal functions and have types:
        -- *Main> :t Giggoer
        -- Giggoer :: String -> String -> [String] -> Bool -> Bool -> User
        -- *Main> :t Venue
        -- Venue :: String -> String -> (Int, Int) -> Int -> Bool -> Bool -> User
-- and return a User

-- Note: type names and constructors MUST start with a Capital Letter

giggoer1 = Giggoer "giggoer1" "giggoer1@gmail.com" [] False True
hydro    = Venue "SSE Hydro" "hydro@sse.co.uk" (0,0) 42 True True

showUser :: User -> String
showUser (Giggoer name email bookmarks pretty_mode dark_mode) = 
             "Name: " ++ name
        ++ ", Email: " ++ email
        ++ ", Bookmarks: " ++ show bookmarks
        ++ ", Pretty Mode: " ++ show pretty_mode
        ++ ", Dark Mode: " ++ show dark_mode
showUser (Venue name email location capacity pretty_mode dark_mode) = 
             "Name: " ++ name
        ++ ", Email: " ++ email
        ++ ", Location: " ++ show location
        ++ ", Capacity: " ++ show capacity
        ++ ", Pretty Mode: " ++ show pretty_mode
        ++ ", Dark Mode: " ++ show dark_mode


                -- Control Structures --

-- Skipping over if-statements and guards
-- One takeaway: if-statements are expressions, and can be used in-line, anywhere
--               whereas guards are NOT expressions (why you'd want guards inline
--               anyway, is a question I cannot answer, Future Me).

-- `case` expressions

f1 0 = 18
f1 1 = 15
f1 2 = 12
f1 x = 12 - x

-- is equivalent to (actually, it's syntactic sugar for)

f2 x = case x of
        0 -> 18
        1 -> 15
        2 -> 12
        _ -> 12 - x

-- case expressions can also bind, not just match:

summarizeString :: String -> String
summarizeString str = case str of
        (_:[])  -> "Just the character " ++ str
        (x:xs)  -> [x] ++ "...(" ++ show (length xs) ++ " more characters)"
        []      -> "Empty string"

-- also, case expressions, as the name would suggest, are expressions. Much like if-statements
-- they can be put in-line anywhere



                -- More on Functions --

-- sidenote about `let` and `where` - `let` is an expression, so it can be used in-line
-- whereas `where`, much like guards, is not.

-- Skipping lambdas, been using that already, except that pattern matching also works here:

lamTail = (\ (_:xs) -> xs)

-- functions whose names are only symbols are *operators* and can be used infix be default, e.g.

(/\) :: [Int] -> Int -> [Int]
xs /\ n = map (n*) xs
        --     ^ this is a "section", and is essentially syntactic sugar for (\ x -> n * x)

        -- [0..10] /\ 10 == [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

-- infix functions can be used as prefix using (function)
-- prefix functions can be used as infix using `function`

-- obviously, only functions with exactly 2 arguments can be made infix

f3 xs = map (\ x -> x * 2 + 3) xs
f4 xs = foldr (\ x y -> read x + y) 1 xs 



                -- Higher-order Functions --

-- for-loop implementation is Haskell
for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i while inc task = do
        task i
        if while (inc i) then for (inc i) while inc task else return ()

-- Some built-in higher-order functions:
        -- flip - swaps arguments of 2-argument functions, e.g. (flip (-)) 2 5 == 5 - 2
        -- (.) - function composition
        -- ($) - application (apply function to argument); very low precedence
        -- uncurry - make function that accepts a series of arguments into one that accepts a tuple
        -- curry - do the opposite
                -- uncurry :: (a -> b -> c) -> (a, b) -> c
                -- curry :: ((a, b) -> c) -> a -> b -> c

        -- id - returns the value passed to it
        -- const - restuns the first value passed to it
                -- Frequently used as arguments TO higher-order functions (as dummy functions)

myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f x y = f (x,y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x,y) = f x y

myConst :: a -> b -> a
myConst x y = x
