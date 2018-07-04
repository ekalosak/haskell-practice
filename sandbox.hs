-- ghci, :l sandbox.hs, :r, minInt
import Data.List
import System.IO

minInt = minBound :: Int
-- Double for decimal values
-- Bool for True, False
-- Char '
-- Tuple


always5 :: Int
always5 = 5 -- never can change
-- always5 = 6 -- will raisse error (Multiple declarations

sumNum = sum [1..1000]
mod5 = mod 5 3
mod4 = 5 `mod` 4
negSum = 5 + (-4)

-- :t sqrt will tell you the type sig of the sqrt function
-- :t (+)
-- :t (**)
nine = 3 ** 2
or = True || False
extended = [1..4] ++ [7..10]
numList = 2 : 5 : 7 : []
multlist = [[2,3], [4,5]]
ll = length multlist
-- reverse numlist, null numlist, numList !! 1, null [], head numList,
-- take 3 extended, drop 4 extended, 6 `elem` extended, elem 4 [1..]
-- product extended, [2,4..32], take 10 (repeat 2), replicate 10 3
-- take 10 (cycle [1..3])
x2 = [x*2 | x <- [1,3..10]]
x3 = [x*3 | x <- [1..100], x*3 <= 100]
x4 = [x | x <- [1..500], x `mod` 14 == 0, x `mod` 3 == 0]
sr = sort [5,4..1]

z = zipWith (+) [1..5] (take 5 [2,4..])
k = filter (>10) z
q = takeWhile (<=20) [1,3..]
f = foldl (*) 1 k
p = [3^x | x <- [1..10]]

-- list is [a], tuple is (a,b)
-- fst ("foo", 7) is "foo", zip [1..5] (take 5 ['A'..]) is [(1,'A'),..]

main = do
    putStrLn "What's up?"
    line <- getLine
    putStrLn ("This is what's up: " ++ line)

addme :: Int -> Int -> Int
addme x y = x + y

foo :: Int -> String
foo 7 = "This is 7"
foo _ = "This is not 7"

fact :: Int -> Int
fact 1 = 1
fact n = n * fact (n-1)

factOfFirst5 = map fact (take 5 [1..])
sumOfFact = sum (map fact [1..5])

isOdd :: Int -> Bool
isOdd xx
    | mod xx 2 == 0 = False
    | otherwise = True

ratio :: Int -> Int -> String
ratio a b
    | c < 5 = "Butt"
    | (c>=5) && (c<10) = "Gut"
    | otherwise = "Dude"
    where c = a + b
-- ratio 2 2 -> "butt"

listfx :: (Show a) => [a] -> String
listfx [] = "Empty"
listfx (x:[]) = show x
listfx (x:xs) = show x ++ " and more stuff"

caps :: String -> String
caps "" = "Empty"
caps all@(x:xs) = "First of " ++ all ++ " is " ++ show x

