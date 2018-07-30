module Sort (insertion_sort, bubble_sort, merge_sort) where

insertion_sort, bubble_sort, merge_sort :: Ord a => [a] -> [a]

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

bubble_map :: Ord a => [a] -> [a]
bubble_map [] = []
bubble_map [x] = [x]
bubble_map xs =
    let
        x1 = min (xs !! 0) (xs !! 1)
        x2 = max (xs !! 0) (xs !! 1)
    in
        x1 : bubble_map (x2 : (tail . tail) xs)

merge_sort [] = []
merge_sort [x] = [x]

is_sorted :: Ord a => [a] -> Bool
is_sorted [] = True
is_sorted [x] = True
is_sorted (x:xs) = and [x <= (head xs), is_sorted xs]
