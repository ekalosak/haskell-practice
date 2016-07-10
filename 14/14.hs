-- Duplicate the items in a list

dupl :: [a] -> [a]
dupl xs
    | length xs == 0 = []
    | otherwise =
        let x:_ = xs in replicate 2 x ++ dupl (tail xs)
