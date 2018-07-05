-- rndSel "asdfgh" 2 = "sg"

import System.Random -- for random number generators
import Control.Monad (replicateM) -- replM n f applies f n times
-- so more or less we use replM to apply the IO monad contained random number
-- generator n times

-- getElm :: IO Int -> [a] -> IO a
-- getElm 0 (x:xs) = return x
-- getElm n (x:xs) = getElm (n-1) xs

rand :: Int -> Int -> IO Int
rand x y = getStdRandom $ randomR (x, y) -- use splitting, not nec. stat. random

rands :: Int -> Int -> Int -> IO [Int]
rands x y n = replicateM n (rand x y)

-- rndSel :: [a] -> Int -> IO [a]
-- rndSel _ 0 = return []
-- rndSel xs n =
--     rands 0 l n >>= (\i -> return [xs !! i])
--     where l = (length xs) - 1
--
--
-- randElm :: [a] -> [IO a]
-- randElm [] = return []
-- randElm xs =
--     let
--         rr = randomR (0, (length xs)-1)
--         y = getStdRandom rr
--     in
--     y >>= (\z -> xs !! z)
