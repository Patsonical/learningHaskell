        -- Declaration
data Tree a = NIL | Branch a (Tree a) (Tree a) deriving (Show)

getVal :: Tree a -> a
getVal (Branch val _ _) = val

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
insert ins this@(Branch val left right)
        | ins == val    = this
        | ins < val     = Branch val (insert ins left) right
        | ins > val     = Branch val left (insert ins right)

insertMultiple :: (Ord a) => [a] -> Tree a -> Tree a
insertMultiple [] tree = tree
insertMultiple (x:xs) tree = insertMultiple xs (insert x tree)

        -- BST Find
find :: (Ord a) => a -> Tree a -> Tree a
find item NIL = NIL
find item this@(Branch val left right)
        | item == val   = this
        | item < val    = find item left
        | item > val    = find item right

        -- BST Deletion

del :: (Ord a) => Tree a -> Tree a
del (Branch val left NIL)       = left
del (Branch val NIL right)      = right
del (Branch val left right)     = Branch min left (delete min right)
        where min = getVal (minTree right)

delete :: (Ord a) => a -> Tree a -> Tree a
delete item NIL = NIL
delete item this@(Branch val left right)
        | item == val   = del this
        | item < val    = Branch val (delete item left) right
        | item > val    = Branch val left (delete item right)
