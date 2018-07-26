-- prmRng 5 11 -> [5,7,11]

module Pr39 where
import Pr31

prmRng :: Int -> Int -> [Int]
prmRng x y =
    if y <= x then []
    else
        [z | z <- [x..y], isprime z]
