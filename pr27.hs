-- g [1, 2] "abc" -> [["a", "bc"], ["b", "ac"], ["c", "ab"]]
g :: [Int] -> [a] -> [[[a]]]

g [] _ = [[[]]]
g _ [] = [[[]]]
-- g ns xs = split ns (perm xs)

-- take e.g. 2 from list, then apply recursively to leftovers

-- h 2 "abc" -> ["ab" "c", "bc" leftovers, "ac" leftovers]
h :: Int -> [a] -> [([a], [a])]
h 0 _ = []
h 1 (x:xs) = ([x], xs) : h 1

-- splitAt 2 [1..5] -> [1,2] [3,4,5]

-- get all ~permutations of base string
-- for each of those, split at e.g. [1,2]

-- perm :: [a] -> [[a]]
-- perm [] = []
-- perm [x] = [x]
-- perm (x:xs) = 
