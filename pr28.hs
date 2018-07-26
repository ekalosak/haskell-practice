import List
import Data.Ord (comparing)

s = [[1,2,3], [1,2], [5]]

-- lsort [[1,2,3], [1,2], [5]] -> [[5], [1,2], [1,2,3]]
lsort :: [[a]] -> [[a]]

lsort [[]] = [[]]
