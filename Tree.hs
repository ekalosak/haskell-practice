module Tree (Tree(Empty, Branch), leaf, lchild, rchild, value, depth, nnodes) where

-- source: 99 haskell problems
data Tree a = Empty | Branch a (Tree a) (Tree a)
              -- deriving (Show)

-- leaf 'x' == leaf 'y' i.e. (==) respects only structure
instance Eq (Tree a) where
    Empty == Empty = True
    Branch x lc rc == Branch x3 lc2 rc2 = (and [lc == lc2, rc == rc2])
    _ == _ = False

instance Show a => Show (Tree a) where
    show Empty = "Empty"
    show (Branch x Empty Empty) = "leaf " ++ show x
    show (Branch x lc rc) = show x ++ "(" ++ show lc ++ "," ++ show rc ++ ")"

-- -- leaf 'x' == leaf 'x' i.e. force value equality testing when possible
-- instance Eq a => Eq (Tree a) where
--     Empty == Empty = True
--     Branch x lc rc == Branch x2 lc2 rc2 =
--         (and [x == x2, lc == lc2, rc == rc2])
--     _ == _ = False

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

-- children :: Tree a -> [Tree a]
-- children = <*> [lchild, rchild]

nnodes :: Tree a -> Int
nnodes Empty = 0
nnodes (Branch _ lc rc) = 1 + (nnodes lc) + (nnodes rc)
