-- gold 28 -> (5, 23)
-- goldbach conjecture that all n > 2 is n = p + q for prime p, q; even n

module Pr40 (gold) where
import Pr31
import Pr35
import Text.Printf

gold :: Int -> (Int, Int)
gold n =
    if not (n `mod` 2 == 0) then
        error (printf "%d not even" n)
    else
        let
            ps = takeWhile (< (n `div` 2)) primes
            ss = filter (\x -> isprime (n-x)) ps
            s = head ss -- lazily evaluated -> only apply filter until accepted
        in
            (s, n-s)

-- issumprime n p = isprime (n-p)
