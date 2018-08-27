-- equidistant neighbors in layout

-- module Pr65 (layout) where
module Pr65 where
import Tree(Tree(Branch, Empty), leaf, depth, lchild, value)

-- external use function

layout :: Tree a -> Tree ((Int, Int), a)
layout Empty = Empty

layroot :: Tree a -> Tree ((Int, Int), a)
layroot Empty = Empty
layroot (Branch x lc rc) = let rd = depth (Branch x lc rc) in
                            (Branch
                                ((1, w), x)
                                (layleft lc 1 w rd)
                                (layright rc 1 w rd)
                            )

-- layleft lc pary parx rootdepth
layleft :: Tree a -> Int -> Int -> Int -> Tree ((Int, Int), a)
layleft Empty _ _ _ = Empty
layleft lc py px rd = (Branch
                        ((py + 1, px - 2^(rd-py)), value lc)
                        (layleft (lchild lc) )
                    )

test_tree =
    Branch 'x'
        (Branch 'y'
            (leaf 'c')
            Empty)
        (Branch 'z'
            (leaf 'a')
            (leaf 'b'))
-- *Pr64> layout test_tree
