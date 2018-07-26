-- gray 2 -> ["00", "01", "10", "11"]

import Control.Monad (replicateM)

gray :: Int -> [[Char]]
gray 0 = []
gray n = replicateM n "01"
