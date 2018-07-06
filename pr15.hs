-- repli "asdf" 3 = "aaasssdddfff"

repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli xs 1 = xs
repli xs n = (xs >>= (\x -> replicate n x))
