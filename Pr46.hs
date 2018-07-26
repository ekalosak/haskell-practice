module Pr46 (table) where
import Prelude hiding (and, or, nand, nor, xor, impl, equ)

-- and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2

and, or, nand, nor, xor, impl, equ :: Bool -> Bool -> Bool

and True True = True
and _ _ = False

or False False = False
or _ _ = True

nand u v = not $ and u v

nor u v = not $ or u v

xor u v = u /= v

equ u v = u == v

impl u v = or (not u) v

-- table
lcs = [(True, True), (True, False), (False, True), (False, False)]

table :: (Bool -> Bool -> Bool) -> IO()
table f = mapM_ putStrLn [show(fst x) ++ " " ++ show(snd x) ++ " " ++
     show(f (fst x) (snd x)) | x <- lcs]
