-- pfacm 100 -> [(2, 2), (5, 2)]

module Pr36 (pfacm) where
import Pr35

pfacm :: Int -> [(Int, Int)]
pfacm 0 = []
pfacm n =
    let
        ps = pfacs' n
        ups = unique ps
    in
        countEach ups ps

-- count 'a' "aabbc" -> 2
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x ys = length [z | z <- ys, z == x]

-- countEach "ab" "aabbc" -> [('a', 2), ('b', 2)]
countEach :: Eq a => [a] -> [a] -> [(a, Int)]
countEach [] _ = []
countEach _ [] = []
countEach [x] ys = [(x, count x ys)]
countEach (x:xs) ys = (x, count x ys) : countEach xs ys
