-- length of a list
-- f :: (Num b) => [a] -> b
f :: [a] -> Int
f [] = 0
f xs = 1 + f (tail xs)
