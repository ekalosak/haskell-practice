module Pr33 (c) where
import Pr32

-- cop 4 6 -> True
-- cop 4 9 -> False
-- determine whether two integers are coprime i.e. if their gcd is 1 or not

c :: Int -> Int -> Bool
c a b = Pr32.g a b == 1
