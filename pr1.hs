-- Problem 1, get last element of a list

f :: [a] -> a
f [] = error "Empty list provided, requires length > 0"
f (x:[]) = x
f (x:xs) = f xs
