-- insat "A" "qwer" 2 = "qAwer"

insat :: a -> [a] -> Int -> [a]
insat y xs 1 = y:xs
insat y (x:xs) n = x:(insat y xs (n-1))

-- e.g. *Main> insat 'A' "qwer" 2
