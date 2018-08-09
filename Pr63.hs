-- construct complete binary tree with k nodes
-- i.e. each level is filled before subsequent level is inserted into

module Pr63 where
import Tree(Tree(Branch, Empty), leaf)

cbt :: Integral a => a -> Tree b
cbt 0 = Empty
cbt n = insert 'x' (cbt n-1)

insert :: a -> Tree a -> Tree a
insert x 
