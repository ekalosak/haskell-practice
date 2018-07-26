-- euler totient function i.e. e(10) -> 1,3,5,7 -> 4

module Pr34 (e) where
import Pr33

e :: Int -> Int
e 0 = 0
e n = length $ coprimes n

coprimes n = filter (\x -> Pr33.c x n) [1..n]
