-- pfacs 20 -> [2, 5]

module Pr35 where
import Pr31 -- isqrt, isprime
import Pr33 -- c -- coprime
import Pr32 -- g -- gcd

pfacs :: Int -> [Int]
pfacs n =
    if isprime n then [n] else
    let
        ps = takeWhile (<= (isqrt n)) primes
        p = head $ filter (\x -> not (c x n)) ps
    in p:(pfacs $ div n p)

-- usage e.g. take 4 primes -> [2,3,5,7]
primes = filter isprime [2..]
