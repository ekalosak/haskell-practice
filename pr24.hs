-- rndNs 3 10 = [1, 7, 2]

import System.Random
import Control.Monad

r :: Int -> Int -> IO Int
r x y = getStdRandom $ randomR (x, y) -- use splitting, not nec. stat. random

rs :: Int -> Int -> Int -> IO [Int]
rs x y n = replicateM n (r x y)

rnd :: Int -> Int -> IO [Int]
rnd n m =
    let
        xs = [1..m]
    in
        rs 0 (m-1) n >>= (\is -> return [xs !! i | i <- is])
