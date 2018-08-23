-- equidistant neighbors in layout

module Pr65 (layout) where
import Tree(Tree(Branch, Empty), leaf, depth, min_depth, nnodes)

-- -- external use function
layout :: Tree a -> Tree (a, (Int, Int))
layout = lay 0 0

-- take a tree at depth n and mark each node with y and x location
-- *helper function*
-- include parent's x and y
lay :: Int -> Int -> Tree a -> Tree (a, (Int, Int))
lay _ _ Empty = Empty
lay d w (Branch x lc rc) = let w2 = 2^(max (depth lc) (depth rc)) in
    (Branch
        (x, (d, w2 + w))
        (lay (d+1) (div (w2+w) 2) lc)
        (lay (d+1) (3*(div (w2+w) 2)) rc)
    )

test_tree = Branch 'x'
        (Branch 'y' (leaf 'c') Empty)
        (Branch 'z' (leaf 'a') (leaf 'b'))
-- *Pr64> layout test_tree
