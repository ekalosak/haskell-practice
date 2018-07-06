-- split "asdf" 2 = "as", "df"

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x:xs) n =
    let yy = split xs (n-1) in
        ([x] ++ fst yy, snd yy)
