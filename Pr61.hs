-- count leaves

module Pr61 where
-- module Pr61 (nleaves) where
import Tree (Tree(Branch, Empty), leaf, lchild, rchild, nodes)

nleaves :: Tree a -> Int
nleaves = length . filter is_leaf . nodes

get_leaves :: Tree a -> [Tree a]
get_leaves = filter is_leaf . nodes
