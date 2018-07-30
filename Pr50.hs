-- Huffman encoding
-- huffman [('a', 24), ('b', 10), ('c', 3)] ->
--          [('a', "0"), ('b', "10"), ('c', "100")]
-- huff "go go gophers" -> [('g', "00"), ('o', "01"), (' ', "110"),
-- ('p', "111"), ('h', "100") ..]

huff :: Eq a => [a] -> [(a, [Char])]
huff [] = []

-- count "aaabbba" -> [('a', 3), ('b', 3), ('a', 1)]
count :: Eq a => [a] -> [(a, Int)]
count [] = []
count [x] = [(x, 1)]
count (x:xs) =
    if x == (fst . head) cxs
    then (x, (snd . head) cxs + 1):(tail cxs)
    else (x, 1):cxs
    where cxs = count xs

-- sequential sort i.e. O(n^2)
sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:xs) =
    let
        y = head ys
        yst = tail ys
    in
    if x < y
    then x:ys
    else y:(x:yst)
    where ys = sort xs
