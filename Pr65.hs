-- equidistant neighbors in layout

-- module Pr65 (layout) where
module Pr65 where
import Tree(Tree(Branch, Empty), leaf, depth, lchild, value)

-- external use function

layout :: Tree a -> Tree ((Int, Int), a)
layout Empty = Empty
layout t = revheight 1 (laywidthroot (layheight t) 0)

-- helper methods

revheight :: Int -> Tree ((Int, Int), a) -> Tree ((Int, Int), a)
revheight _ Empty = Empty
revheight ph (Branch ((h, w), x) lc rc) =
    (Branch ((ph, w), x) (revheight (ph+1) lc) (revheight (ph+1) rc))

layheight :: Tree a -> Tree (Int, a)
layheight Empty = Empty
layheight (Branch x lc rc) =
    let m = max (depth lc) (depth rc) in
    (Branch (m, x) (layheight lc) (layheight rc))

laywidthroot :: Tree (Int, a) -> Int -> Tree ((Int, Int), a)
laywidthroot Empty _ = Empty
laywidthroot (Branch (h, x) lc rc) p =
    let pw = 2^h in
    (Branch ((h, pw), x) (laywidthl lc pw) (laywidthr rc pw))

laywidthl :: Tree (Int, a) -> Int -> Tree ((Int, Int), a)
laywidthl Empty _ = Empty
laywidthl (Branch (h, x) lc rc) p =
    let pw = p - 2^h in
    (Branch ((h, pw), x) (laywidthl lc pw) (laywidthr rc pw))

laywidthr :: Tree (Int, a) -> Int -> Tree ((Int, Int), a)
laywidthr Empty _ = Empty
laywidthr (Branch (h, x) lc rc) p =
    let pw = p + 2^h in
    (Branch ((h, pw), x) (laywidthl lc pw) (laywidthr rc pw))

test_tree =
    Branch 'x'
        (Branch 'y'
            (leaf 'c')
            Empty)
        (Branch 'z'
            (leaf 'a')
            (leaf 'b'))
-- *Pr65> layout test_tree
