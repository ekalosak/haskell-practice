-- (rotate '(a b c d e f g h) 3) = (D E F G H A B C)

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n
    | n > 0     = rotate (xx ++ [x]) (n-1)
    | otherwise = reverse(rotate (reverse xs) (-n))
    where
        xx = tail(xs)
        x = head(xs)
