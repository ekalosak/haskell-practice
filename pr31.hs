-- isPrime 7 -> True

-- primes :: Int -> [Int]
-- primes 1 = []
-- primes 2 = [2]
-- primes n = [] -- CTN here

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
