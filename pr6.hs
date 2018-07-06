-- detect palindrome
f :: (Eq a) => [a] -> Bool
f [] = True
f xs = if length xs == 1
    then True
    else
        if (head xs) == (last xs)
        then (f (init (tail xs)))
        else False

-- Example: f "abccbadabccba"
-- Example: f $ [1..5] ++ (reverse [1..5])
