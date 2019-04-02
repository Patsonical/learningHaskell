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