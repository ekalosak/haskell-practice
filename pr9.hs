-- pack [1,1,1,2,2,3,3,1] -> [[1,1,1],[2,2],[3,3],[1]]

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = let y = h1 xs in [fst y] ++ pack(snd y)

h1 :: (Eq a) => [a] -> ([a], [a])
h1 [] = ([], [])
h1 (x:xs) =
    if xs == [] then ([x], []) else
    if x == head(xs)
    then let y = h1 xs in ([x] ++ fst(y), snd(y))
    else ([x], xs)

-- ex: pak "aaaabbbbcccccccceed"
