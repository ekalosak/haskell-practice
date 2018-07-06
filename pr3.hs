-- find k'th element of a list
f :: (Num b, Eq b) => [a] -> b -> a

f [] _ = error "Index out of bounds"
f xs 1 = head xs
f xs k = f (tail xs) (k-1)
