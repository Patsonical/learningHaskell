                -- Applicative Functors --

import Text.Read

-- using the readMaybe function
-- readMaybe :: Read a => String -> Maybe a
-- This function takes a string and turns it into a Maybe value
-- i.e. it parses the string and returns Just if the string can
-- be interpreted as a certain value, or Nothing if it can't
        -- e.g.
        -- readMaybe "3" :: Maybe Integer
        --   > Just 3
        -- readMaybe "three" :: Maybe Integer
        --   > Nothing
-- The ":: Maybe Integer" is called a type annotation, and tells
-- the function which type it should evaluate to (in this case,
-- Maybe Integer)

interactiveDouble = do
        putStrLn "Please enter a number:"
        input <- getLine
        let maybeNum = readMaybe input :: Maybe Double
        case maybeNum of
                Just x -> putStrLn (show (x) ++ " times two is " ++ show (2*x))
                Nothing -> do
                        putStrLn ("Cannot resolve " ++ input ++ " as a number")
                        interactiveDouble

        -- The doubling could have been done before unwrapping the Maybe,
        -- using fmap (since Maybe is a Functor)

-- What if we wanted to apply a function to multiple Maybe values?
-- Say, to sum two Maybe Double's for instance?
        -- fmap (+) (Just 3) :: Num a => Maybe (a -> a)
-- fmap won't work, because it yields a function, wrapped in a Maybe
-- How are we supposed to apply that???
-- We would need someFunction :: Maybe (a -> b) -> Maybe a -> Maybe b

-- Enter Applicative Functors <*>

-- fmap (+) (Just 3) <*> (Just 4)
--   > Just 7
        -- OR
-- (+) <$> (Just 3) <*> (Just 4)
--   > Just 7
        -- Where <$> is the same thing as `fmap` - its infix synonym

-- <*> is actually more general than just working on Maybes
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--              /\
--        New Type Class: Applicative Functors

-- instance Applicative Maybe where
--      pure                  = Just
--      (Just f) <*> (Just x) = Just (f x)
--      _        <*> _        = Nothing

-- The second and third definitions are simple enough: if both values
-- are Just, then apply f to x and wrap it in a Just, and if either
-- of them is Nothing, give back a Nothing.
-- So what's with pure?

-- pure :: Applicative f => a -> f a
-- pure takes a value and brings it into the functor in the
-- default way for the functor. In the case of Maybe, wrap
-- the value in a Just.
-- There are laws that dictate what a default or trivial way of
-- bringing a value into the functor means, which will be covered later
--      (p.269 - Haskell WikiBook)

-- Now we can write a proper interactive summing function
interactiveSum = do
        putStrLn "Enter two numbers:"
        n1 <- getLine
        n2 <- getLine
        let     m1 = readMaybe n1 :: Maybe Double
                m2 = readMaybe n2
        case (+) <$> m1 <*> m2 of
                Just x  -> putStrLn (n1 ++ " + " ++ n2 ++ " = " ++ show x)
                Nothing -> do
                        putStrLn "Some of the values entered are not numbers"
                        interactiveSum


                -- IO --

