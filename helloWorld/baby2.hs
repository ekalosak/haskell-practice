initials :: [Char] -> [Char] -> [Char]
initials (f:_) (l:_) = (f : ". ") ++ (l : ".")
