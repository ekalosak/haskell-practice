-- g 25 16 -> 4

g :: Int -> Int -> Int
g 0 n = n
g n 0 = n
g a b =
    let
        x = max a b
        y = min a b
    in
        g (x-y) y
