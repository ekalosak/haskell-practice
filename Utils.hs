module Utils (tuplein, tuplecomp) where

tuplecomp :: (Eq a, Show a) => (a, a) -> a -> a
tuplecomp t n
    | n == fst t    = snd t
    | n == snd t    = fst t
    | otherwise     = -- error "tuple does not contain thing"
        error ("<" ++ show t ++ "> does not contain <" ++ show n ++ ">")

-- <tuplein t n> is true if <n> is in <t>
tuplein :: Eq a => (a, a) -> a -> Bool
tuplein t n
    | n == fst t    = True
    | n == snd t    = True
    | otherwise     = False
