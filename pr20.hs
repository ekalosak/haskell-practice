-- remv "asdfg" 3 = "as" "fg" -> "asfg"

remv :: [a] -> Int -> [a]
remv [] _ = []
remv xs 0 = xs
remv (x:xs) 1 = xs
remv (x:xs) n = x:(remv xs (n-1))
