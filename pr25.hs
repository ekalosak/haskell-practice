-- (rnd-permu '(a b c d e f))
-- (B A D C E F)'

import System.Random
import Control.Monad

rperm :: [a] -> IO [a]
rperm [] = return []
rperm (x:xs) =
    do
        r <- randomRIO (0, (length xs) - 1)
        xsr <- rperm xs
        return $
            let (ys, zs) = splitAt r xsr
            in ys++(x:zs)
