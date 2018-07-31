module Sort (insertion_sort, bubble_sort, merge_sort, quick_sort) where

import Debug.Trace

quick_sort, insertion_sort, bubble_sort,
    merge_sort :: (Ord a, Show a) => [a] -> [a]

insertion_sort [] = []
insertion_sort [x] = [x]
insertion_sort (x:xs) =
    if x < head ys
    then x:ys
    else (head ys) : insertion_sort (x:(tail ys))
    where ys = insertion_sort xs

bubble_sort [] = []
bubble_sort [x] = [x]
bubble_sort xs =
    if is_sorted xs
    then xs
    else (bubble_sort . bubble_map) xs

bubble_map :: (Ord a, Show a) => [a] -> [a]
bubble_map [] = []
bubble_map [x] = [x]
bubble_map xs =
    let
        x1 = min (xs !! 0) (xs !! 1)
        x2 = max (xs !! 0) (xs !! 1)
        rest = drop 2 xs
    in
        let result = x1 : bubble_map (x2 : rest) in
        -- trace (show result) result
        result

-- broken mergesort TODO: complete implementation
merge_sort [] = []
merge_sort [x] = [x]
merge_sort [x, y] = merge ([x], [y])
merge_sort xs =
    let
        n = length xs
        xst = splitAt (div n 2) xs
        result = merge $ tmap merge_sort xst
    in
        trace (show result) result

merge :: (Ord a, Show a) => ([a], [a]) -> [a]
merge ([],  []) = []
merge (xs,  []) = xs
merge ([],  xs) = xs
merge (xs,  ys) =
    let result = if (head xs) < (head ys)
        then (head xs) : merge ((tail xs), ys)
        else (head ys) : merge (xs, (tail ys))
    in trace (show result) result

tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f xst = (f $ fst xst, f $ snd xst)

is_sorted :: Ord a => [a] -> Bool
is_sorted [] = True
is_sorted [x] = True
is_sorted (x:xs) = and [x <= (head xs), is_sorted xs]

quick_sort [] = []
