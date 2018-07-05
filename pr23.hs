-- rndSel "asdfgh" 2 = "sg"

import System.Random -- for random number generators
import Control.Monad (replicateM) -- replM n f applies f n times
-- so more or less we use replM to apply the IO monad contained random number
-- generator n times

rand :: Int -> Int -> IO Int
rand x y = getStdRandom $ randomR (x, y) -- use splitting, not nec. stat. random

rands :: Int -> Int -> Int -> IO [Int]
rands x y n = replicateM n (rand x y)

rndSel :: [a] -> Int -> IO [a]
rndSel _ 0 = return []
rndSel xs n =
    rands 0 l n >>= (\is -> return [xs !! i | i <- is])
    where l = (length xs) - 1
