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

        -- General maps and folds

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
