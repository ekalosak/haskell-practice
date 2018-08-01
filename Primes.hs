-- different ways to generate primes

ps :: Int -> [Int]
ps 2 = [2]
ps n =
    let pn1 = ps (n-1) in
    if any (==0) $ map (rem n) pn1
    then pn1
    else pn1 ++ [n]

isqrt :: Integral a => a -> a
isqrt = ceiling . sqrt . fromIntegral

ispm :: Int -> Bool
ispm 1 = True
ispm 2 = True
ispm n = null [q | q <- [x | x <- [2..(isqrt n)], ispm x], rem n q == 0]
