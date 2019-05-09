        -- Declaration
data Tree a = NIL | Branch a (Tree a) (Tree a) deriving (Show)

        -- Test tree
testTree = Branch 7 (Branch 5 (Branch 2 NIL NIL) (Branch 6 NIL NIL)) (Branch 10 (Branch 8 NIL NIL) NIL)

        -- Traversals
preorder :: Tree a -> [a]
preorder NIL = []
preorder (Branch val left right) = [val] ++ (preorder left) ++ (preorder right)

inorder :: Tree a -> [a]
inorder NIL = []
inorder (Branch val left right) = (inorder left) ++ [val] ++ (inorder right)

postorder :: Tree a -> [a]
postorder NIL = []
postorder (Branch val left right) = (postorder left) ++ (postorder right) ++ [val]

        -- Get min and max elements
minTree :: (Ord a) => Tree a -> Tree a
minTree NIL = NIL
minTree (Branch val NIL _)  = Branch val NIL NIL
minTree (Branch val left _) = minTree left

maxTree :: (Ord a) => Tree a -> Tree a
maxTree NIL = NIL
maxTree (Branch val _ NIL)  = Branch val NIL NIL
maxTree (Branch val _ right) = maxTree right

        -- BST Insertion
insert :: (Ord a) => a -> Tree a -> Tree a
insert ins NIL = Branch ins NIL NIL
insert ins (Branch val left right)
        | ins == val    = Branch val left right
        | ins < val     = Branch val (insert ins left) right
        | ins > val     = Branch val left (insert ins right)

        -- BST Deletion
delete :: (Ord a) => a -> Tree a -> Tree a
delete del NIL = NIL
