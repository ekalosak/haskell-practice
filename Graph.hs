module Graph (Graph, Node) where
-- module Graph (Graph(Node, Empty)) where
-- module Graph (Graph(Empty, Node, Edge)) where

data Node a = Node a deriving Eq
-- graph-term form
data Graph a = Graph [Node a] [(Node a, Node a)] deriving Eq
-- edge-term form
data GraphE a = GraphE [(Node a, Node a)] deriving (Show, Eq)

instance Show a => Show (Node a) where
    show (Node x) = show x

instance Show a => Show (Graph a) where
    show (Graph nodes edges) = show edges

x = Node 'x'
y = Node 'y'
e = (x, y)
g = Graph [x, y] [e]
