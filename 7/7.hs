{- define a nexted list,
 - then the flattenting function
 - test on b by running :l 7 in the ghci
 -
 - The algorithm is: if elm, -> elm, if list -> elms and if elm in elms is list
 - then self(elm).
 - -}

 -- Type declaration: nested list of a's is a list of a's and nested lists
 --
 -- NOTE: following works: list1 x = [x]; concatMap list1 [1..5]

import Data.Tree

t = Node 1 [Node 2 [Node 3 []]]
f = flatten t
