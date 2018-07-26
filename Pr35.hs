-- pfacs 20 -> [2, 5]

module Pr35 (pfacs', pfacs, unique, primes) where
import Pr31 -- isqrt, isprime
import Pr33 -- c -- coprime
import Pr32 -- g -- gcd

pfacs' :: Int -> [Int]
pfacs' n =
    if isprime n then [n] else
    let
        ps = takeWhile (<= (isqrt n)) primes
        p = head $ filter (\x -> not (c x n)) ps
    in p:(pfacs' $ div n p)

-- usage e.g. take 4 primes -> [2,3,5,7]
primes = filter isprime [2..]

pfacs :: Int -> [Int]
pfacs n = (unique . pfacs') n

unique :: Eq a => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x:xs) = x : unique [y | y <- xs, y /= x]
