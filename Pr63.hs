-- construct complete binary tree with k nodes
-- i.e. each level is filled before subsequent level is inserted into
-- and bottom depth is left adjusted

module Pr63 (cbt) where
import Tree(Tree(Branch, Empty), leaf, depth, lchild, rchild, min_depth)

-- NOTE: THIS solution does not work

cbt :: Int -> a -> Tree a
cbt 0 _ = Empty
cbt n x = ins x $ cbt (n-1) x

ins :: a -> Tree a -> Tree a
ins x Empty = leaf x
ins x (Branch y lc rc)
    | ml == mr  = (Branch y (ins x lc) rc)
    | otherwise = (Branch y lc (ins x rc))
    where
        ml = min_depth lc
        mr = min_depth rc
