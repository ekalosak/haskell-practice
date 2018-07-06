-- second to last elements of list
f :: [a] -> a

f [] = error "Empty list provided, requires length > 0"
f (x:y:[]) = x
f (x:xs) = f xs
