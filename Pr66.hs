-- compressed neighbors in layout

module Pr66 (layout) where
import Tree(Tree(Branch, Empty), leaf, depth, lchild, value)

-- external use function

layout :: Tree a -> Tree ((Int, Int), a)
layout Empty = Empty

-- Strategy:
-- 1. initialize x and y values
-- 2. look for conflicts in locations
-- 3. adjust conflicts' x values at deepest common parent

-- layout the y values
layy = layyh 0

-- y layout helper function (takes tree and parent depth)
layyh :: Int -> Tree a -> Tree (Int, a)
layyh _ Empty = Empty
layyh pd (Branch x lc rc) = let d = pd + 1 in
    Branch (d, x) (layyh d lc) (layyh d rc)

tolist :: Tree a -> [a]
tolist Empty = []
tolist (Branch x lc rc) = x:((tolist lc) ++ (tolist rc))

-- initialize x value
-- layxinit = layxinith 0
-- layxinith :: Int -

test_tree =
    Branch 'n'
        (Branch 'k'
            (Branch 'c'
                (leaf 'a')
                (Branch 'e'
                    (leaf 'd')
                    (leaf 'g')
                ))
            (leaf 'm'))
        (Branch 'u'
            (Branch 'p'
                Empty
                (leaf 'q'))
            Empty)

-- *Pr66> layout test_tree
