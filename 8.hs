-- Eliminate duplicate chars in str
compress :: [Char] -> [Char]

compress str
    | length str == 0 = ""
    | length xs == 0 = [x]
    | otherwise =
        if x == head xs
        then compress xs
        else x : compress xs
     where x:xs = str
