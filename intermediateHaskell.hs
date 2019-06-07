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
