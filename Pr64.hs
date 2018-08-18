-- prepare tree drawing layout
-- xy of lc is lower and to left of parent and to left of all rc's children

module Pr64 (lay) where
import Tree(Tree(Branch, Empty), leaf, depth, min_depth)

lay :: Tree a -> Tree (a, (Int, Int))
lay Empty = Empty
lay (leaf x) = leaf (x, (1, 1))
