-- split string
split :: [a] -> Int -> ([a], [a])
split xs n
    | length xs == 0 = ([], [])
    | n == 0 = ([], xs)
    | otherwise =
        split xxs (n-1)
        where x:xxs = xs
