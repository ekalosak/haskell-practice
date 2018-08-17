-- construct complete binary tree with k nodes
-- i.e. each level is filled before subsequent level is inserted into

module Pr63 where
import Tree(Tree(Branch, Empty), leaf, depth, lchild, rchild)

-- NOTE: THIS solution does not work

cbt :: Integral a => a -> b -> Tree b
cbt 0 _ = Empty
cbt n x = insert x (cbt (n-1) x)

insert :: a -> Tree a -> Tree a
insert x Empty = leaf x
insert x (Branch y lc rc)
    | dl < dr
    | and [is_bal lc, is_bal rc] =
        Branch y (insert x lc) rc
    | and [is_bal lc, not . is_bal $ rc] =
        Branch y lc (insert x rc)
    | and [not . is_bal $ lc, is_bal rc] =
        Branch y lc (insert x rc)
    where
        dl = depth lc
        dr = depth rc

is_bal :: Tree a -> Bool
is_bal Empty = True
is_bal (Branch x lc rc) = and [
        depth lc - depth rc == 0,
        is_bal rc, is_bal lc
    ]

is_alm_bal :: Tree a -> Bool
is_alm_bal Empty = True
-- fails to check that lc is full before inserting into rc
is_alm_bal (Branch x lc rc) = and [
        abs (depth lc - depth rc) <= 1,
        is_alm_bal lc,
        is_alm_bal rc
    ]
