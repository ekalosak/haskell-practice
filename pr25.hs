-- (rnd-permu '(a b c d e f))
-- (B A D C E F)'

import System.Random
import Control.Monad

-- random Int in range
r :: Int -> Int -> IO Int
r x y = getStdRandom $ randomR (x, y)

-- random Ints in range, n of them
rs :: Int -> Int -> Int -> IO [Int]
rs x y n = replicateM n (r x y)

-- core functionality
rp :: [a] -> IO [a]
rp [] = return []
-- rp xs =

-- pure function permuting elements n <-> m in list xs
perm :: [a] -> Int -> Int -> [a]
perm xs n m =
    let
        u = min n m
        v = max n m
        x1 = xs !! u
        x2 = xs !! v
        (xs1, xsr) = splitAt u xs
        (xs2, xs3) = splitAt (v-u) xsr
    in
        xs1 ++ [x2] ++ tail(xs2) ++ [x1] ++ tail(xs3)
