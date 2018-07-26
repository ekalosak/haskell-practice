-- g [1, 2] "abc" -> [["a", "bc"], ["b", "ac"], ["c", "ab"]]
g :: [Int] -> [a] -> [[[a]]]

g [] _ = [[[]]]
g _ [] = [[[]]]

-- c gets list of combinations of elements and the elements excluded therefrom
c :: Int -> [a] -> [([a], [a])]
c 0 xs = [([],xs)]
