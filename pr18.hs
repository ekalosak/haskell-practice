-- slice "asdfghj" 2 4 = "sdf"

slice :: [a] -> Int -> Int -> [a]

slice [] _ _ = []
slice (x:xs) 1 1 = [x]
slice (x:xs) 1 m = x:(slice xs 1 (m-1))
slice (x:xs) n m = slice xs (n-1) (m-1)

-- if n >= m then [] else
