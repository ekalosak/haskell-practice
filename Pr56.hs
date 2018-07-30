-- test whether a tree is symmetric

import Tree

mirror :: Eq a => Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror x y =
    if (value x) == (value y)
    then and [mirror (rchild x) (lchild y), mirror (lchild x) (rchild y)]
    else False

sym :: Eq a => Tree a -> Bool
sym Empty = True
sym x = mirror (rchild x) (lchild x)
