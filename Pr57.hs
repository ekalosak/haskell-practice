-- build a binary search tree

module Pr57 (stree, balt) where

import Tree

-- NOTE: this doesn't handle (==) conditions correctly
stree :: Ord a => [a] -> Tree a
stree [] = Empty
stree [x] = Leaf x
stree (x:xs) =
    let
        right = filter (>x) xs
        left = filter (<=x) xs
    in
        Branch x (stree left) (stree right)

balt :: Tree a -> Bool
balt Empty = True
balt t = and [
    ((depth . rchild) t) == ((depth . lchild) t),
    (balt . rchild) t,
    (balt . lchild) t
    ]
