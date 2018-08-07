-- get internal nodes

module Pr62 where
import Tree (Tree(Branch, Empty), is_leaf, nodes, leaf)

internal_nodes :: Tree a -> [Tree a]
internal_nodes = filter (not . is_leaf) . nodes
