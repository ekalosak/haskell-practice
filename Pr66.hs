-- compressed neighbors in layout

module Pr66 (layout) where
import Tree(Tree(Branch, Empty), leaf, depth, lchild, value)

-- Strategy:
-- 1. initialize x and y values
-- 2. look for conflicts in locations
-- 3. adjust conflicts' x values at deepest common parent


layout :: Tree a -> Tree ((Int, Int), a)
-- TODO: The following is not correct. include the conflict adjusting recursive
-- functions.
layout = finaladj . layxinit . layy

finaladj = finalyadjust . finalxadjust

-- move y so that miny is 1
finalyadjust :: Tree ((Int, Int), a) -> Tree ((Int, Int), a)
finalyadjust Empty = Empty
finalyadjust tr = adjy 0 tr

-- move x so that minx is 1
finalxadjust :: Tree ((Int, Int), a) -> Tree ((Int, Int), a)
finalxadjust Empty = Empty
finalxadjust tr = adjx (minxt tr) tr

minxt :: Tree ((Int, Int), a) -> Int
minxt = foldl1 min . minxth

minxth :: Tree ((Int, Int), a) -> [Int]
minxth Empty = []
minxth (Branch ((x, y), k) lc rc) = x:((minxth lc) ++ (minxth rc))

adjx :: Int -> Tree ((Int, Int), a) -> Tree ((Int, Int), a)
adjx _ Empty = Empty
adjx d (Branch ((x, y), k) lc rc) =
    (Branch ((x-d, y), k) (adjx d lc) (adjx d rc))

adjy :: Int -> Tree ((Int, Int), a) -> Tree ((Int, Int), a)
adjy _ Empty = Empty
adjy d (Branch ((x, y), k) lc rc) =
    (Branch ((x, y+d), k) (adjy d lc) (adjy d rc))

-- layout the y values
layy = layyh 0

-- y layout helper function (takes tree and parent depth)
layyh :: Int -> Tree a -> Tree (Int, a)
layyh _ Empty = Empty
layyh pd (Branch x lc rc) = let d = pd + 1 in
    Branch (d, x) (layyh d lc) (layyh d rc)

tolist :: Tree a -> [a]
tolist Empty = []
tolist (Branch x lc rc) = x:((tolist lc) ++ (tolist rc))

-- initialize x value
layxinit = layxinith 0

-- x layout helper
layxinith :: Int -> Tree (Int, a) -> Tree ((Int, Int), a)
layxinith _ Empty = Empty
layxinith x (Branch (y, k) lc rc) =
    (Branch ((x, y), k) (layxinith (x-1) lc) (layxinith (x+1) rc))

test_tree =
    Branch 'n'
        (Branch 'k'
            (Branch 'c'
                (leaf 'a')
                (Branch 'e'
                    (leaf 'd')
                    (leaf 'g')
                ))
            (leaf 'm'))
        (Branch 'u'
            (Branch 'p'
                Empty
                (leaf 'q'))
            Empty)

-- *Pr66> layout test_tree
