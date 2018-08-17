-- test equality of lists

module Listeq (lin, leq) where

f :: Eq a => (a, a) -> Bool
f = (\x -> (fst x) == (snd x))

leq :: Eq a => [a] -> [a] -> Bool
-- leq = all . (\x -> (fst x) == (snd x)) . zip
-- or
-- leq [] [] = True
-- leq [] _ = True
-- leq _ [] = True
-- leq (x:xs) (y:ys) = (x == y) && leq xs ys
-- or
-- leq = (\x -> and . zipWith (==) x)
-- or
leq = (and .) . zipWith (==)

-- a = [1..5]
-- b = [1..5]
-- zip a b
-- zip a $ b
-- map f :: Eq a => [(a, a)] -> Bool

lin :: Eq a => a -> [a] -> Bool
lin = any . (==)
