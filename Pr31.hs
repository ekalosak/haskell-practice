-- isPrime 7 -> True

module Pr31 (isqrt, isprime) where

-- uses square root seive without memoization
isprime :: Int -> Bool
isprime 0 = False
isprime 1 = True
isprime n =
    if (length $ filter iscoprime ps) == 0
    then True
    else False
    where
        iscoprime = (\x -> n `mod` x == 0)
        ps = [2..(isqrt n)]

isqrt = floor . sqrt . fromIntegral
