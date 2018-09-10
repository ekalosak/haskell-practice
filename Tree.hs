module Tree (Tree(Empty, Branch),
    leaf, lchild, rchild, value, depth, nnodes, nodes, children, is_leaf,
    is_full, min_depth, empty, test_tree) where

-- source: 99 haskell problems
data Tree a = Empty | Branch a (Tree a) (Tree a)
              -- deriving (Show)

-- leaf 'x' == leaf 'y' i.e. (==) respects only structure
instance Eq (Tree a) where
    Empty == Empty = True
    Branch x lc rc == Branch x3 lc2 rc2 = (and [lc == lc2, rc == rc2])
    _ == _ = False

instance Show a => Show (Tree a) where
    show Empty = "Empty"
    show (Branch x Empty Empty) = "leaf " ++ show x
    show (Branch x lc rc) = show x ++ "(" ++ show lc ++ "," ++ show rc ++ ")"

empty :: Tree a -> Bool
empty Empty = True
empty _ = False

nodes :: Tree a -> [Tree a]
nodes Empty = []
nodes tr = tr:((nodes $ lchild tr) ++ (nodes $ rchild tr))

-- -- leaf 'x' == leaf 'x' i.e. force value equality testing when possible
-- instance Eq a => Eq (Tree a) where
--     Empty == Empty = True
--     Branch x lc rc == Branch x2 lc2 rc2 =
--         (and [x == x2, lc == lc2, rc == rc2])
--     _ == _ = False

leaf x = Branch x Empty Empty

-- tree_ex = Branch 1 (leaf 2) Empty

rchild, lchild :: Tree a -> Tree a

rchild Empty = Empty
rchild (Branch _ _ x) = x

lchild Empty = Empty
lchild (Branch _ x _) = x

value :: Tree a -> a
value (Branch x _ _) = x

depth :: Tree a -> Int
depth Empty = 0
depth (Branch x l r) = 1 + max (depth l) (depth r)

-- children :: Tree a -> [Tree a]
-- children = <*> [lchild, rchild]

nnodes :: Tree a -> Int
nnodes Empty = 0
nnodes (Branch _ lc rc) = 1 + (nnodes lc) + (nnodes rc)

children :: Tree a -> [Tree a]
children Empty = []
children (Branch _ lc rc) = [lc, rc]

is_leaf :: Tree a -> Bool
-- is_leaf = and . (==Empty) <$> children
is_leaf (Branch _ Empty Empty) = True
is_leaf _ = False

-- whether tree is completely balanced i.e. all subtree depths are ==
is_full :: Tree a -> Bool
is_full Empty = True
is_full (Branch _ lc rc) = and [is_full lc, is_full rc, depth lc == depth rc]

-- minimum depth of leaves in a tree
min_depth :: Tree a -> Int
min_depth Empty = 0
min_depth (Branch x lc rc) = 1 + (min (min_depth lc) (min_depth rc))
--
-- width of a tree is just the number of nodes in it for this layout
width :: Tree a -> Int
width = nnodes

test_tree =
    Branch 'n'
        (Branch 'k'
            (Branch 'c'
                (leaf 'a')
                (Branch 'e'
                    (leaf 'd')
                    (leaf 'g')
                ))
            (leaf 'm'))
        (Branch 'u'
            (Branch 'p'
                Empty
                (leaf 'q'))
            Empty)
