-- construct hbal trees with n nodes (rather than d depth as in Pr59)
module Pr60 where

import Tree (Tree(Branch, Empty), nnodes)
import Pr59 (is_hbal, all_trees_and_smaller)

hbal :: a -> Int -> [Tree a]
hbal x n = filter is_hbal [t | t <- all_trees_and_smaller n x, nnodes t == n]
