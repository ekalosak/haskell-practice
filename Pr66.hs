-- compressed neighbors in layout

module Pr66 (layout) where
import Tree(Tree(Branch, Empty), leaf, depth, rchild, lchild, value, empty,
    test_tree)

-- Strategy:
-- 1. initialize x and y values
-- 2. look for conflicts in locations
-- 3. adjust conflicts' x values at deepest common parent


layout :: Tree a -> Tree ((Int, Int), a)
-- TODO: The following is not correct. include the conflict adjusting recursive
-- functions.
layout = finaladj . fixconflict . layxinit . layy

fixconflict :: Tree ((Int, Int), a) -> Tree ((Int, Int), a)
fixconflict t = if conflict t then fixconflict $ adjconflict t else t

adjconflict :: Tree ((Int, Int), a) -> Tree ((Int, Int), a)
adjconflict t
    | (empty t)                 = Empty
    | ((not lcc) && (not rcc))  = (Branch v (adjx 1 lc) (adjx (-1) rc))
    | otherwise                 = (Branch v (adjconflict lc) (adjconflict rc))
    where
        v = value t
        lc = lchild t
        lcc = conflict lc
        rc = rchild t
        rcc = conflict rc
    -- if ((conflict))

-- determine whether there is a conflict in the locations of a tree
conflict :: Tree ((Int, Int), a) -> Bool
conflict = unique . toloclist

-- are elements of a list unique? useful in determining location conflicts
unique :: Eq a => [a] -> Bool
unique = or . uniqueh
uniqueh [] = []
uniqueh (x:xs) = (x `elem` xs):(uniqueh xs)

toloclist :: Tree ((Int, Int), a) -> [(Int, Int)]
toloclist Empty = []
toloclist (Branch ((x, y), k) lc rc) = (x,y):((toloclist lc) ++ (toloclist rc))

-- aesthetic adjustment that normalizes x and y relative to (1,1)
finaladj = finalyadjust . finalxadjust

-- move y so that miny is 1
finalyadjust :: Tree ((Int, Int), a) -> Tree ((Int, Int), a)
finalyadjust Empty = Empty
finalyadjust tr = adjy 0 tr

-- move x so that minx is 1
finalxadjust :: Tree ((Int, Int), a) -> Tree ((Int, Int), a)
finalxadjust Empty = Empty
finalxadjust tr = adjx (minxt tr - 1) tr

-- get min x value in tree
minxt :: Tree ((Int, Int), a) -> Int
minxt = foldl1 min . minxth

-- flatten tree to list of x values
minxth :: Tree ((Int, Int), a) -> [Int]
minxth Empty = []
minxth (Branch ((x, y), k) lc rc) = x:((minxth lc) ++ (minxth rc))

-- adjust all x values by d
adjx :: Int -> Tree ((Int, Int), a) -> Tree ((Int, Int), a)
adjx _ Empty = Empty
adjx d (Branch ((x, y), k) lc rc) =
    (Branch ((x-d, y), k) (adjx d lc) (adjx d rc))

-- adjust all y values by d
adjy :: Int -> Tree ((Int, Int), a) -> Tree ((Int, Int), a)
adjy _ Empty = Empty
adjy d (Branch ((x, y), k) lc rc) =
    (Branch ((x, y+d), k) (adjy d lc) (adjy d rc))

-- initialize layout
initlayout :: Tree a -> Tree ((Int, Int), a)
initlayout = layxinit . layy

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

-- *Pr66> layout test_tree
t = initlayout test_tree
