module Tree (Tree(Empty, Branch), leaf) where

-- source: 99 haskell problems
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf x = Branch x Empty Empty

tree_ex = Branch 1 (leaf 2) Empty
