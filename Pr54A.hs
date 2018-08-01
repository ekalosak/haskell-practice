-- isTree expression -> boolean
-- isTree (A (B nil nil) nil) -> True
-- isTree (A nil nil) -> True
-- isTree (A (B nil) nil) -> False

import Data.Typeable

data NestedList a = Elem a | List [NestedList a] deriving (Show)

isTree :: (Typeable a) => NestedList a -> Bool
isTree (Elem x) = False
-- isTree (List xs) =
--     if length xs
