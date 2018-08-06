-- generate all symmetric, balanced trees of n-nodes = <k>
-- using the "generate and test" paradigm
-- i.e. propose all symmetric trees and check which ones are balanced

import Tree (Tree(Empty, Branch), leaf, rchild, lchild)

bal_sym_trees :: Int -> [Tree]
bal_sym_trees n = filter is_symmetric (all_trees n)

is_symmetric :: Tree -> Bool
is_symmetric Empty = True
is_symmetric Branch x lc rc = (rc == mirror_tree lc)

mirror_tree :: Tree -> Tree
mirror_tree Empty = Empty
mirror_tree Branch x lc rc = Branch x (mirror_tree rc) (mirror_tree lc)

all_trees :: Int -> [Tree]
all_trees 0 = []
