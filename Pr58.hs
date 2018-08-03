-- generate all symmetric, balanced trees of n-nodes = <k>
-- using the "generate and test" paradigm
-- i.e. propose all symmetric trees and check which ones are balanced

import Trees

balsymts :: Int -> [Tree]
balsymts n = filter balanced (symts n)

balanced :: Tree -> Bool
balanced = balt -- just a synonym for a function in module Pr57

symts :: Int -> [Tree]
symts 0 = []
symts 1 = 
