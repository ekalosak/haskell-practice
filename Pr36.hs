-- pfacm 100 -> [(2, 2), (5, 2)]

module Pr36 (pfacm) where
import Pr35

pfacm :: Int -> [(Int, Int)]
pfacm 0 = []
-- pfacm n =
--     let
--         ps = pfacs' n
--         s = length [x | x <- ps, x == head ps] -- e.g. 100 -> [2,2,5,5] -> [2,2]
--     in
--         (head ps, s) : (pfacm . head . snd) $ splitAt s ps

-- count 'a' "aabbc" -> 2
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x ys = length [z | z <- ys, z == x]
