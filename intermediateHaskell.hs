import FirstModule
import qualified ModuleDir.SecondModule as Module2

                -- Other Data Structures --

        -- Trees (recursive data structure)

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)
                                        -- Don't worry about deriving yet
treeToList (Leaf x) = [x]
treeToList (Branch x y) = treeToList x ++ treeToList y

exampleTree = Branch (Leaf 7) (Branch (Leaf 3) (Leaf 4))

        -- Tree Map

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch x y) = Branch (treeMap f x) (treeMap f y)

extreeTimesTwo = treeMap (2*) exampleTree

        -- Tree Fold

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold fbranch fleaf = g where
        -- so g `sometree` = treeFold fbranch fleaf `sometree`
        -- this is to avoid having to write "treeFold fbranch fleaf" repeatedly
        g (Leaf x) = fleaf x
        g (Branch left right) = fbranch (g left) (g right)

extreeSum = treeFold (+) id exampleTree         -- id = (\x -> x)
extreeList = treeFold (++) (: []) exampleTree   -- (: []) = (\x -> [x])

        -- General Maps

data Weird a b = First a
               | Second b
               | Third [(a,b)]
               | Fourth (Weird a b)
               deriving (Show)

weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = g where
        g (First x)  = First (fa x)
        g (Second y) = Second (fb y)
        g (Third z)  = Third [ (fa x,fb y) | (x,y) <- z ]
-- or, using map     = Third ( map (\(x,y) -> (fa x,fb y)) z)
        g (Fourth w) = Fourth (g w)

t1 = First 1
t2 = Second 'a'
t3 = Third [(2,'b'),(3,'c'),(4,'d')]
t4 = Fourth (Second 'e')
t5 = Fourth (t3)
tmap = weirdMap (10*) (\x -> "Char: " ++ [x])

        -- General Folds

weirdFold :: (a -> c) -> (b -> c) -> ([(a,b)] -> c) ->
             (c -> c) -> Weird a b -> c

weirdFold f1 f2 f3 f4 = g where
        g (First x)  = f1 x
        g (Second y) = f2 y
        g (Third z)  = f3 z
        g (Fourth w) = f4 (g w)


                -- Classes and Types --

        -- Instantiation

data EqExample = EqEx {x :: Integer, str :: String}

--      Class
--       \/
instance Eq EqExample where
        (EqEx x1 str1) == (EqEx x2 str2) = (x1 == x2) && (str1 == str2)
--                                              Integer and String are members of Eq

        -- Deriving

data DerExample = DerEx Integer String
        deriving (Eq, Ord, Show)

-- this automatically generates required functions for Eq Ord and Show
-- Deriving only works with a couple of built-in classes:
        -- Eq, Ord, Enum, Bounded, Show, Read

-- Classes can inherit from other classes, meaning that
-- for something to be an instance of a class, it also
-- has to be an instance of the parent class.
-- Classes can inherit from multiple classes.


