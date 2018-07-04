-- range 3 8 = [3 4 5 6 7 8]

range :: Int -> Int -> [Int]
range n m
    | n == m        = [m]
    | n > m         = []
    | otherwise     = n:(range (n+1) m)
