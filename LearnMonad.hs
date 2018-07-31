-- applicative (<*> is pronounced "app" or "applicative")
-- instance Applicative f for a Functor f
--  has methods [pure, (<*>)]

gs = [(*2), (+4), (\x -> 6*x - 10)]
xs = [2..4]

app_out = gs <*> xs


-- bind (>>=) takes f :: a -> (b, c) and g :: b -> (d, c)
-- and combines them like (g >>= f) x = (g.(fst f) x, c)
-- where (g >>= f) :: a -> (d, c)

-- f :: Int -> (Float, [Char])
-- f x = (3.0, "did f")
--
-- g :: Float -> ([Char], [Char])
-- g y = (show y, "did g")

h :: Int -> Maybe [Int]
h x = if x < 6 then Just $ replicate 3 x else Nothing

k :: [Int] -> Maybe Int
k xs = let s = sum xs in if s >= 10 then Just s else Nothing

-- (h 4) >>= k -- h 4 -> Just [4,4,4], then >>= takes Just [4,4,4] to [4,4,4]
--             -- and feeds that to k as in "k [4,4,4] -> Just 16"
--
-- (h 3) >>= k -- h 3 gives Just [3,3,3] but sum of that is 9 so k bound to that
--             -- result gives Nothing
--
-- (h 6) >>= k -- h 6 gives Nothing so k bound to it should also give nothing
