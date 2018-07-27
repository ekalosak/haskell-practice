-- gray 2 -> ["00", "01", "10", "11"]

import Control.Monad (replicateM)

gray :: Int -> [[Char]]
gray 0 = []
gray 1 = ["0", "1"]
gray n = [x++y | x <- gray 1, y <- gray (n-1)]

--
-- gray n = replicateM n "01"
