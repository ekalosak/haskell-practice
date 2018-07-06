-- (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))')

-- From prs 9 and 10
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

-- Pr 11, define new data type akin to nestedlist from pr8
data Tupor a = Single a | Multiple a Int
    deriving (Show)

h1 :: (Eq a) => (a, Int) -> Tupor a
h1 (x, i) = if i == 1 then Single x else Multiple x i

h3 :: (Eq a) => [(a, Int)] -> [Tupor a]
h3 [] = []
h3 xs = [h1(head xs)] ++ (h3(tail xs))

encmod :: (Eq a) => [a] -> [Tupor a]
encmod xs = h3 $ encode xs
