-- construct complete binary tree with k nodes
-- i.e. each level is filled before subsequent level is inserted into
-- and bottom depth is left adjusted

module Pr63 where
import Tree(Tree(Branch, Empty), leaf, depth, lchild, rchild)

-- NOTE: THIS solution does not work

cbt :: Int -> a -> Tree a
cbt 0 _ = Empty
cbt n x = ins x $ cbt (n-1) x

-- nodes at depth h have 2^(h-1) nodes
-- e.g. depth 1 has 1, d 2 has 2, 3 -> 4, 4 -> 8, etc
-- recursive def'n will split

-- tp = [2^n | n <- [1..]]

-- -- get biggest power of 2 less than or equal to n
-- f :: Int -> Int
-- f n = (head $ filter (>n) tp) `div` 2

ins :: a -> Tree a -> Tree a
ins x Empty = leaf x
ins x (Branch y lc rc)
    | ml == mr  = (Branch y (ins x lc) rc)
    | otherwise = (Branch y lc (ins x rc))
    where
        ml = min_depth lc
        mr = min_depth rc

-- minimum depth of leaves in a tree
min_depth :: Tree a -> Int
min_depth Empty = 0
min_depth (Branch x lc rc) = 1 + (min (min_depth lc) (min_depth rc))

-- whether tree is completely balanced i.e. all subtree depths are ==
is_full :: Tree a -> Bool
is_full Empty = True
is_full (Branch _ lc rc) = and [is_full lc, is_full rc, depth lc == depth rc]
