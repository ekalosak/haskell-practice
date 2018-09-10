-- module containing multi-child trees and accessory functions
module MultTree (Tree(Node), leaf) where
-- module MultTree (Tree(Node), leaf, nnodes) where

-- data types
data Tree a = Node a [Tree a] deriving (Show, Eq)

-- helper functions
leaf x = Node x []

nnodes :: Tree a -> Int
nnodes = sum1 . map nnodes . children

sum1 :: (Foldable t, Num a) => t a -> a
sum1 xs = 1 + sum xs

children :: Tree a -> [Tree a]
children (Node x xs) = xs

-- internal path length is the sum of lengths of paths to root from each node
ipl :: Tree a -> Int
ipl = iplh 0

iplh :: Int -> Tree a -> Int
iplh d (Node x xs) = d + sum (map (iplh (d+1)) xs)

-- test tree source: https://wiki.haskell.org/99_questions/70B_to_73
-- under problem 70B
test_tree = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

-- instance Show a => Show (Tree a) where
--     show (Node x xs) = x ++ "\n" ++ (flatten $ map show xs)
