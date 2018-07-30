-- build a binary search tree

import Tree

stree :: Eq a => [a] -> Tree a
stree [] = Empty
stree [x] = leaf x
stree xs = TODO
