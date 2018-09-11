module Graph (Graph, Node, unique) where

-- -- DATA CONSTRUCTORS
data Node a = Node a deriving Eq
-- graph-term form
data Graph a = Graph [Node a] [(Node a, Node a)] deriving Eq
-- edge-term form
data GraphE a = GraphE [(Node a, Node a)] deriving (Show, Eq)

instance Show a => Show (Node a) where
    show (Node x) = show x

instance Show a => Show (Graph a) where
    show (Graph nodes edges) = show edges

-- -- FUNCTIONS
convert_gtoe :: Graph a -> GraphE a
convert_gtoe (Graph nodes edges) = (GraphE edges)

-- convert_etog :: GraphE a -> Graph a
-- convert_etog (GraphE edges) =
--     let nodes = unique $ extract_nodes edges in
--     Graph nodes edges

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x:(unique [y | y <- xs, y /= x])

-- -- TESTING COMPONENTS
x = Node 'x'
y = Node 'y'
e = (x, y)
g = Graph [x, y] [e]
