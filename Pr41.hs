-- goldbach list: goldl 3 8 -> [(2,2), (3,3), (1,7)]

module Pr41 where
import Pr40 -- gold 28 -> (5, 23)

goldl :: Int -> Int -> [(Int, Int)]
goldl x y =
    let
        u = min x y
        v = max x y
        ns = [n | n <- [u..v], n `mod` 2 == 0]
    in
        map gold ns
