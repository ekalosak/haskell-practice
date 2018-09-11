module Graph (Graph, Node, unique) where

-- -- DATA CONSTRUCTORS
data Node a = Node a deriving Eq
-- graph-term form
-- data GraphG a = Graph a | GraphE a
data Graph a = Graph [Node a] [(Node a, Node a)] deriving Eq
-- edge-term form
data GraphE a = GraphE [(Node a, Node a)] deriving (Show, Eq)

instance Show a => Show (Node a) where
    show (Node x) = show x

instance Show a => Show (Graph a) where
    show (Graph nodes edges) = "Graph <" ++ show edges ++ ">"

-- -- FUNCTIONS
-- conversions
convert_gtoe :: Graph a -> GraphE a
convert_gtoe (Graph nodes edges) = (GraphE edges)

convert_etog :: Eq a => GraphE a -> Graph a
convert_etog (GraphE edges) =
    let nodes = unique $ extract_nodes edges in
    Graph nodes edges

-- basic functionality
nodes :: Graph a -> [Node a]
nodes (Graph ns es) = ns

edges :: Graph a -> [(Node a, Node a)]
edges (Graph ns es) = es

-- helpers
extract_nodes :: [(Node a, Node a)] -> [Node a]
extract_nodes [] = []
extract_nodes edges = flatten $ map (\t -> [fst t, snd t]) edges

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x:(unique [y | y <- xs, y /= x])

-- -- TESTING COMPONENTS
x = Node 'x'
y = Node 'y'
e = (x, y)
g = Graph [x, y] [e]
