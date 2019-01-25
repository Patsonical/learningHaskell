-- Simple Functions --
tripleThis x = 3 * x
isEven x = x `mod` 2 == 0
makeEven x = if isEven x then x else x+1
makeOdd x = if isEven x then x+1 else x
doubleToEven x = if isEven x then x else x*2

-- Recursive Functions: Factorial and Sum --
recursiveFactorial :: (Integral a) => a -> a
recursiveFactorial 0 = 1
recursiveFactorial n = n * recursiveFactorial (n-1)

recursiveSum :: (Num a) => [a] -> a
recursiveSum [] = 0
recursiveSum (x:xs) = x + recursiveSum xs

-- Remove all instances of x from a list --
remove :: (Eq a) => a -> [a] -> [a]
remove x [] = []
remove x (h:t) = if h == x then remove x t else h:(remove x t)
	--remove x l = if (head l) == x then remove x (tail l) else (head l):(remove x (tail l))

-- Patterns (wholeString refers to l:rest) --
firstLetter :: String -> String
firstLetter "" = error "Empty String"
firstLetter wholeString@(l:rest) = "The first letter of " ++ wholeString ++ " is " ++ [l]

-- BMI and Guards --
bmi :: Double -> Double -> Double
bmi weight height = weight / (height^2)

bmiTell :: (RealFloat a, Show a) => a -> String
bmiTell x
	| x <= 18.5	= "BMI <" ++ (show x) ++ "> is underweight"
	| x <= 25.0	= "BMI <" ++ (show x) ++ "> is normal"
	| x <= 30.0	= "BMI <" ++ (show x) ++ "> is overweight"
	| otherwise	= "BMI <" ++ (show x) ++ "> is obese"
	{- ^otherwise is defined <otherwise = True> by default 
	   so this always evaluates to True.
	   In fact, you could just write < | True > instead -}

absolute :: (RealFloat a) => a -> a
absolute x
	| x < 0		= 0 - x
	| otherwise	= x

-- The *where* keyword --
heron :: (RealFloat a) => a -> a -> a -> a
heron a b c = sqrt (s * (s-a) * (s-b) * (s-c))
	where
	s = (a+b+c) / 2

	{- This is Heron's Formula:
	   Area of a triangle with sides a b c -}

-- Guards work with *where* as well --
numOfRealSolutions :: (RealFloat a) => a -> a -> a -> Int
numOfRealSolutions a b c
	| a == 0			= error "Not a quadratic"
	| discriminant > 0	= 2
	| discriminant == 0	= 1
	| otherwise			= 0
		where
		discriminant = b^2 - 4*a*c

-- Quick Sort --
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort ls@(x:xs) = qsort smaller ++ equal ++ qsort larger
	where
		smaller = [s | s <- ls, s <  x]
		larger  = [l | l <- ls, l >  x]
		equal   = [e | e <- ls, e == x]

-- Pattern Matching: fst/snd for triplets --
fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

snd3 :: (a,b,c) -> b
snd3 (_, x, _) = x

trd3 :: (a,b,c) -> c
trd3 (_, _, x) = x

-- Using *let* as an alternative to *where* --
roots :: (Floating a) => a -> a -> a -> (a, a)
roots a b c =
	let	rtDisc = sqrt(b^2 - 4*a*c) --This (after *let*) could've been a new line--
		twoA = 2*a
	in	((-b + rtDisc) / twoA, --Same here--
		 (-b - rtDisc) / twoA)

-- Function Compositions --
squareThis x = x^2
tripleAndSquare x = squareThis (tripleThis x)
squareAndTriple = tripleThis . squareThis
	{- or, more explicitly:
	   squareAndTriple x = (tripleThis . squareThis) x -}
	-- note that the first version of squareAndTriple *still takes x as an argument*!

	-- btw, I just realised that I don't actually have to put `--` at the end of a comment...
	-- I will still use this format for titles though.

-- Prelude and Importing Modules --
	{- To import modules (libraries), the following syntax is used:
	   `import Data.List`
	   Note that this *has to be at the top* of the file! -}

	-- GHCi can also use `:m +Data.List` as shorthand for this

	{- The `Prelude` core library is loaded by default in
	   every Haskell program, and we can use its functions,
	   as demonstrated below -}

revWords :: String -> String
revWords = unwords . reverse . words
	-- A.K.A. `revWords x = (unwords . reverse . words) x`
	-- or     `revWords x = unwords (reverse (words x))`
	-- or     `revWords x = unwords $ reverse $ words x`
	-- This reverses the order of words in a string using the three functions (from Prelude)
		-- `words` splits a string by whitespace
		-- `reverse` reverses a list
		-- `unwords` joins the [String] into a single string (opposite of `words`)

-- Simple I/O --
	-- Prelude> putStrLn "Hello World!"
	-- Hello World!
	-- Prelude> :t putStrLn
	-- putStrLn :: String -> IO ()


-- Continued in the next file --
