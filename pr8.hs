-- Compress

nnon :: (Eq a) => [a] -> a -> [a]
nnon [] a = []
nnon (x:xs) a = if x == a
            then nnon xs a
            else x:xs

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = [x] ++ (compress (nnon xs x))
