-- drp "asdfasdf" 2 = "adad" # drop every 2nd element

drp :: [a] -> Int -> [a]
drp [] _ = []
drp _ 0 = []
-- drp _ 1 = drop _ 0
drp xs n =
    let l = length xs in
    let r = (div l n) + 1 in
    let ixs = concat(replicate r [1..n]) in
    h1 xs ixs n

h1 :: [a] -> [Int] -> Int -> [a]
h1 [] _ _ = []
h1 (x:xs) (i:is) n =
    if i == n
    then h1 xs is n
    else [x] ++ h1 xs is n
