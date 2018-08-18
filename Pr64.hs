-- prepare tree drawing layout
-- xy of lc is lower and to left of parent and to left of all rc's children

module Pr64 (lay) where
import Tree(Tree(Branch, Empty), leaf, depth, min_depth, nnodes)

lay :: Tree a -> Tree (a, (Int, Int))
lay Empty = Empty
-- lay (Branch x lc rc) = Branch (x, )
-- lay (Branch x lc rc) = Branch (x, )

-- width of a tree is just the number of nodes in it for this layout
width :: Tree a -> Int
width = nnodes
