-- equidistant neighbors in layout

-- module Pr65 (layout) where
module Pr65 where
import Tree(Tree(Branch, Empty), leaf, depth, lchild)

-- -- external use function
-- layout :: Tree a -> Tree (a, (Int, Int))
-- layout = (\t -> lay (getrootyx t) t)

getrootyx :: Tree a -> (Int, Int)
getrootyx t = (depth t, 2^((getlwidth t) - 1))

getlwidth :: Tree a -> Int
getlwidth Empty = 0
getlwidth t = 2^(depth t - 1) + (getlwidth (lchild t))

test_tree =
    Branch 'x'
        (Branch 'y'
            (leaf 'c')
            Empty)
        (Branch 'z'
            (leaf 'a')
            (leaf 'b'))
-- *Pr64> layout test_tree
