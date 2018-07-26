import Pr32

-- cop 4 6 -> True
-- cop 4 9 -> False
-- determine whether two integers are coprime i.e. if their gcd is 1 or not

-- from pr32:

-- g :: Int -> Int -> Int
-- g 0 n = n
-- g n 0 = n
-- g a b =
--     let
--         x = max a b
--         y = min a b
--     in
--         g (x-y) y


c :: Int -> Int -> Bool
c a b = mygcd.g a b == 1