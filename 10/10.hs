{-
Runlength encoding
i.e. 'aaabbaaa' -> [('a',3), ('b',2), ('a', 3)]
-}

-- Single function taking strings to lists of tuples of chars and ints

encode :: String -> [(Char, Int)]
