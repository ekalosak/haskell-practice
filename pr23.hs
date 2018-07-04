-- rndSel "asdfgh" 2 = "sg"

import System.Random -- for random number generators
import Control.Monad (replicateM) -- replM n f applies f n times
-- so more or less we use replM to apply the IO monad contained random number
-- generator n times

-- rndSel :: [a] -> Int -> [a]
-- rndSel xs n =
--     let
--         l = length xs
--         s = newStdGen -- uses splitting behind, not nec. crypt. secure
--     in

-- randElm :: [a] -> [IO a]
-- randElm [] = return []
-- randElm xs =
--     let
--         rr = randomR (0, (length xs)-1)
--         y = getStdRandom rr
--     in
--     y >>= (\z -> xs !! z)

getElm :: IO Int -> [a] -> IO a
getElm 0 (x:xs) = IO x
getElm n (x:xs) = getElm (n-1) xs
