-- Drop every n'th element of a list
drp :: Int -> [a] -> [a]
drp n xs
    | length xs == 0 = []
    | otherwise = (take (n - 1) xs) ++ drp n (drop n xs)
