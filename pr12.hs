-- decodeModified
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--                Multiple 2 'a',Single 'd',Multiple 4 'e']
--                "aaaabccaadeeee"

data Tupor a = Single a | Multiple a Int
    deriving (Show)

decodeModified :: [Tupor a] -> [a]
decodeModified [] = []
decodeModified xs = h1(head xs) ++ decodeModified(tail xs)

-- decm :: [Tupor a] -> a
-- decm xs = concat(decodeModified xs)
--
h1 :: Tupor a -> [a]
h1 (Single x) = [x]
h1 (Multiple x n) = replicate n x

-- Example: y = decodeModified [Multiple "a" 3, Multiple "b" 4, Single "c"]
-- concat y
