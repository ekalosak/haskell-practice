-- combintions 2 "abc" -> [ab, ac, bc]

c :: Int -> [a] -> [[a]]
c 0 _ = [[]]
c _ [] = [[]]
c 1 (x:xs) = [[x]] ++ (c 1 xs)
c n (x:xs) = [[x]]

k :: a -> [a] -> Int -> [[a]]
k x _ 0 = [[x]]
k x [] _ = [[]]
k x (y:ys) 1 = [[x, y]] ++ (k x ys 1)
k x (ys) n = [[x] ++ take n ys]
