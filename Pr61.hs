-- count leaves

module Pr61 where
-- module Pr61 (nleaves) where
import Tree (Tree(Branch, Empty), leaf, lchild, rchild)

nleaves :: Tree a -> Int
nleaves = length . filter is_leaf . nodes

nodes :: Tree a -> [Tree a]
nodes Empty = []
nodes tr = tr:((nodes $ lchild tr) ++ (nodes $ rchild tr))

children :: Tree a -> [Tree a]
children Empty = []
children (Branch _ lc rc) = [lc, rc]

is_leaf :: Tree a -> Bool
-- is_leaf = and . (==Empty) <$> children
is_leaf (Branch _ Empty Empty) = True
is_leaf _ = False

get_leaves :: Tree a -> [Tree a]
get_leaves = filter is_leaf . nodes
