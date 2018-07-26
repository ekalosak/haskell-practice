-- g [1, 2] "abc" -> [["a", "bc"], ["b", "ac"], ["c", "ab"]]

g :: [Int] -> [a] -> [[[a]]]

g [] _ = [[[]]]
g _ [] = [[[]]]
g [1] [x] = [[[x]]]
g [1] (x:xs) = [[x]] : g [1] xs
g [n] [x] = [[[]]] -- n > 1 by failure to match preceeding patern

g [n] (x:xs)
    | n > length ys     = [[[]]]
    | n == length ys    = [[xs]]
    | otherwise         =   -- n < length xs e.g. >>> g 2 "asdf"
        h x (g [n-1] xs) ++ g [n] xs
    where ys = (x:xs)

-- h ["a"] [["bc"], ["bd"]] -> [["a", "bc"], ["a", "bd"]]
h :: [[a]] -> [[[a]]] -> [[[a]]]
h _ [[[]]] = [[[]]]
h [[]] _ = [[[]]]
h [x] [[y]] = [[x], [y]]

-- for all permutations up to equality within subset permutations, take 1 then 2
-- i.e. g [1,2] "abc" -> take 1 2 ["abc", "bac", "cab"]
-- g 2 "abc" -> ["ab", "bc", "ac"]
-- g 1 "abc" -> ["a", "b", "c"]
