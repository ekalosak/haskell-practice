-- get internal nodes

module Pr62 where
import Tree (Tree(Branch, Empty), is_leaf, nodes, leaf, children)

internal_nodes :: Tree a -> [Tree a]
internal_nodes = filter (not . is_leaf) . nodes

nodes_at :: Int -> Tree a -> [Tree a]
nodes_at _ Empty = []
nodes_at 1 tr = [tr]
nodes_at n tr = (>>=) (children tr) (nodes_at (n-1))
-- nodes_at n = (\x -> (children x >>= (nodes_at (n-1))))
