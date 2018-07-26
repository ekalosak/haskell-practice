-- tableg 3 (a and b and (not c)) -> truth table

module Pr48 (tableg) where
import Control.Monad (replicateM)

tableg :: Int -> ([Bool] -> Bool) -> IO ()
tableg 0 _ = putStrLn ""
tableg n f =
    mapM_ putStrLn
    []
