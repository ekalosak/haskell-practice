-- generate all symmetric, balanced trees of n-nodes = <k>
-- using the "generate and test" paradigm
-- i.e. propose all symmetric trees and check which ones are balanced

import Tree (Tree(Empty, Branch), leaf, rchild, lchild)

-- bal_sym_trees :: Int -> [Tree a]
-- bal_sym_trees n = filter is_symmetric (all_trees n)
--
-- is_symmetric :: Tree a -> Bool
-- is_symmetric Empty = True
-- is_symmetric Branch x lc rc = (rc == mirror_tree lc)
--
-- mirror_tree :: Tree a -> Tree a
-- mirror_tree Empty = Empty
-- mirror_tree Branch x lc rc = Branch x (mirror_tree rc) (mirror_tree lc)

all_trees :: Int -> [Tree Char]
all_trees 0 = [Empty]
all_trees 1 = [leaf 'x']
all_trees n = [Branch 'x' t1 t2 | (l1, l2) <- all_splits (n-1),
    t1 <- (all_trees l1),
    t2 <- (all_trees l2)]

all_splits :: Int -> [(Int, Int)]
all_splits n = [(x, y) | x <- [0..n], y <- [0..n], x + y == n]
