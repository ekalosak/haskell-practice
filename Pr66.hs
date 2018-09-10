-- compressed neighbors in layout

module Pr66 (layout) where
import Tree(Tree(Branch, Empty), leaf, depth, lchild, value)

-- external use function

layout :: Tree a -> Tree ((Int, Int), a)
layout Empty = Empty


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
