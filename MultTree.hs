module MultTree (Tree(Node)) where

data Tree a = Node a [Tree a] deriving (Show, Eq)
