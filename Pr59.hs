-- hbal n x -> Tree where each node has property depth lc - depth rc <= 1

module Pr59 (is_hbal, hbal, all_trees_and_smaller) where

import Tree -- depth, Tree(Branch, Empty)
import Pr58 -- all_trees

hbal :: Int -> a -> [Tree a]
hbal n x = [t | t <- (all_trees_and_smaller (n^2) x),
    depth t == n, is_hbal t]

hbal' :: Int -> a -> [Tree a]
hbal' n x = filter (is_hbal) (all_trees n x)

is_hbal :: Tree a -> Bool
is_hbal Empty = True
is_hbal (Branch x lc rc) = and $ (aeq_depth lc rc):(map is_hbal [lc, rc])

aeq_depth :: Tree a -> Tree b -> Bool
aeq_depth t1 t2 = abs ((depth t1) - (depth t2)) <= 1

all_trees_and_smaller :: Int -> a -> [Tree a]
all_trees_and_smaller 0 _ = []
all_trees_and_smaller n x = (all_trees n x) ++ (all_trees_and_smaller (n-1) x)
