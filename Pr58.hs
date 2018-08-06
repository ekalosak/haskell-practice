-- generate all symmetric, balanced trees of n-nodes = <k>
-- using the "generate and test" paradigm
-- i.e. propose all symmetric trees and check which ones are balanced

module Pr58 (all_trees, all_splits, mirror_tree, is_symmetric, bal_sym_trees)
where

import Tree (Tree(Empty, Branch), leaf, rchild, lchild, depth)

bal_sym_trees :: Eq a => Int -> a -> [Tree a]
bal_sym_trees n x = filter ((&&) <$> is_symmetric <*> is_balanced)
    (all_trees n x)

-- <$> is fmap

is_balanced :: Tree a -> Bool
is_balanced Empty = True
is_balanced (Branch x lc rc) =
    and $ ((abs ((depth lc) - (depth rc))) <= 1):(map is_balanced [lc, rc])

is_symmetric :: Eq a => Tree a -> Bool
is_symmetric Empty = True
is_symmetric (Branch x lc rc) = (rc == mirror_tree lc)

mirror_tree :: Tree a -> Tree a
mirror_tree Empty = Empty
mirror_tree (Branch x lc rc) = Branch x (mirror_tree rc) (mirror_tree lc)

all_trees :: Int -> a -> [Tree a]
all_trees 0 _ = [Empty]
all_trees 1 x = [leaf x]
all_trees n x = [Branch x t1 t2 | (l1, l2) <- all_splits (n-1),
    t1 <- (all_trees l1 x),
    t2 <- (all_trees l2 x)]

all_splits :: Int -> [(Int, Int)]
all_splits n = [(x, y) | x <- [0..n], y <- [0..n], x + y == n]
