-- prepare tree drawing layout
-- xy of lc is lower and to left of parent and to left of all rc's children

module Pr64 (lay) where
import Tree(Tree(Branch, Empty), leaf, depth, min_depth, nnodes)

-- -- external use function
layout :: Tree a -> Tree (a, (Int, Int))
layout = lay 0 0

-- take a tree at depth n and mark each node with y and x location
-- *helper function*
lay :: Int -> Int -> Tree a -> Tree (a, (Int, Int))
lay _ _ Empty = Empty
lay d w (Branch x lc rc) =
    Branch
        (x, (d+1, (width lc) + 1))
        (lay (d+1) lc)
        (lay (d+1) rc)

-- NOTE: This solution does not yet work

-- width of a tree is just the number of nodes in it for this layout
width :: Tree a -> Int
width = nnodes
