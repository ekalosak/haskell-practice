module Tree (Tree(Empty, Branch), leaf, lchild, rchild, value) where

-- source: 99 haskell problems
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf x = Branch x Empty Empty

-- tree_ex = Branch 1 (leaf 2) Empty

rchild, lchild :: Tree a -> Tree a

rchild Empty = Empty
rchild (Branch _ _ x) = x

lchild Empty = Empty
lchild (Branch _ x _) = x

value :: Tree a -> a
value (Branch x _ _) = x

depth :: Tree a -> Int
depth Empty = 0
depth (Branch x l r) = 1 + max (depth l) (depth r)
