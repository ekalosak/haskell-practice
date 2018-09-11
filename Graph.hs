module GraphG (GraphG, Node, unique) where

-- -- DATA CONSTRUCTORS
data Node a = Node a deriving Eq
-- graph-term form
data GraphG a = GraphG [Node a] [(Node a, Node a)] deriving Eq
-- edge-term form
data GraphE a = GraphE [(Node a, Node a)] deriving (Show, Eq)
-- adjacency form
data GraphA a = GraphA [(Node a, [Node a])] -- list of (node, connected nodes)
-- TODO: convert between GraphA and the other two
-- TODO: validation functions for these forms

instance Show a => Show (Node a) where
    show (Node x) = show x

instance Show a => Show (GraphG a) where
    show (GraphG nodes edges) = "GraphG: \n\t" ++
        show nodes ++ "\n\t" ++ show edges

-- -- FUNCTIONS
-- conversions
convert_gtoe :: GraphG a -> GraphE a
convert_gtoe (GraphG nodes edges) = (GraphE edges)

convert_etog :: Eq a => GraphE a -> GraphG a
convert_etog (GraphE edges) =
    let nodes = unique $ extract_nodes edges in
    GraphG nodes edges

-- basic functionality
nodes :: GraphG a -> [Node a]
nodes (GraphG ns es) = ns

edges :: GraphG a -> [(Node a, Node a)]
edges (GraphG ns es) = es

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
g = GraphG [x, y] [e]
