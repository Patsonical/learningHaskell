import FirstModule
import qualified ModuleDir.SecondModule as Module2

                -- Other Data Structures --

        -- Trees (recursive data structure)

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)
                                        -- Don't worry about deriving yet
treeToList (Leaf x) = [x]
treeToList (Branch x y) = treeToList x ++ treeToList y

exampleTree = Branch (Leaf 7) (Branch (Leaf 3) (Leaf 4))

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch x y) = Branch (treeMap f x) (treeMap f y)

extreeTimesTwo = treeMap (2*) exampleTree

        -- Hmm, that tree only stores data in the leaves...
        -- Let's make a proper binary tree!

data BinTree a = BtLeaf a | Subtree a (BinTree a) (BinTree a) deriving (Show)

binTreeMap :: (a -> b) -> BinTree a -> BinTree b
binTreeMap f (BtLeaf val) = BtLeaf (f val)
binTreeMap f (Subtree val left right) = Subtree (f val) (binTreeMap f left) (binTreeMap f right)

exampleBinTree = Subtree 2 (Subtree 1 (BtLeaf 5) (BtLeaf 6)) (BtLeaf 4)

ebtTimesTen = binTreeMap (10*) exampleBinTree

-- NEXT: TREE FOLDS (or skip that and move onto general maps and folds)
