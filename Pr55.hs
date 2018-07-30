-- make a completely balanced tree with N elements

import Tree

x = 'x'

balt :: Int -> Tree Char
balt 0 = Empty
balt 1 = leaf x
balt 2 = Branch x (leaf x) Empty
balt 3 = Branch x (leaf x) (leaf x)
balt n =
    if n `mod` 2 == 1
    then Branch x bn2 bn2
    else Branch x bn2 bn21
    where
        bn2 = balt (n `div` 2)
        bn21 = balt ((n `div` 2) - 1)
