-- combintions 2 "abc" -> [ab, ac, bc]

-- c 3 "abcd" -> ["abc", "abd", "acd", "bcd"]
c :: Int -> [a] -> [[a]]
c 0 _ = [[]]
c _ [] = [[]]
c 1 [x] = [[x]]
c 1 (x:xs) = [[x]] ++ (c 1 xs)
c n (x:xs) =
    if length xs > n-1
    then (h x (c (n-1) xs)) ++ (c n xs)
    else (h x (c (n-1) xs))

-- h 'a' ["bc", "cd", "bd"] -> ["abc", "acd", "abd"]
h :: a -> [[a]] -> [[a]]
h _ [] = [[]]
h x (y:ys) =
    if length ys > 0
    then [x:y] ++ (h x ys)
    else [x:y]
