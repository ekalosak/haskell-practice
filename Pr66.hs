-- compressed neighbors in layout

module Pr66 (layout) where
import Tree(Tree(Branch, Empty), leaf, depth, lchild, value)

-- external use function

layout :: Tree a -> Tree ((Int, Int), a)
layout Empty = Empty


