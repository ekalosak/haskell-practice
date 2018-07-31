-- build a binary search tree

import Tree

stree :: Ord a => [a] -> Tree a
stree [] = Empty
stree [x] = leaf x
stree (x:xs) =
    let
        right = filter (>x) xs
        left = filter (<=x) xs
    in
        Branch x (stree left) (stree right)
