-- encode "aaabbcc" -> [("a", 3) ("b", 2) ("c", 2)]

-- too lazy to make and compile from modules via interdependencies

-- from: pr9.hs
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = let y = cut xs in [fst y] ++ pack(snd y)

cut :: (Eq a) => [a] -> ([a], [a])
cut [] = ([], [])
cut (x:xs) =
    if xs == [] then ([x], []) else
    if x == head(xs)
    then let y = cut xs in ([x] ++ fst(y), snd(y))
    else ([x], xs)

encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode xs = let (y, ys) = cut xs in
    [(head y, length y)] ++ encode ys
