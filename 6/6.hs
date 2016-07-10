myPal :: Eq a => [a] -> Bool
myPal l = if l == reverse l then True else False
