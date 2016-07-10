-- Duplicate the items in a list a specified number of times

repl :: Int -> [a] -> [a]
repl n xs
    | length xs == 0 = []
    | otherwise =
        let x:_ = xs in replicate n x ++ repl n (tail xs)
