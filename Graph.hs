module Graph (GraphG, GraphE, GraphA, Node, unique) where

-- -- DATA CONSTRUCTORS
data Node a = Node a deriving Eq
data Edge a = Edge (Node a, Node a) deriving Eq
-- graph-term form
-- data GraphG a = GraphG [Node a] [(Node a, Node a)] deriving Eq
data GraphG a = GraphG [Node a] [Edge a] deriving Eq
-- edge-term form
-- data GraphE a = GraphE [(Node a, Node a)] deriving Eq
data GraphE a = GraphE [Edge a] deriving Eq
-- adjacency form
data GraphA a = GraphA [(Node a, [Node a])] deriving Eq
-- TODO: convert between GraphA and the other two
-- TODO: validation functions for these forms

instance Show a => Show (Node a) where
    show (Node x) = show x

instance Show a => Show (Edge a) where
    show (Edge (Node x, Node y)) = "<" ++ show x ++ "," ++ show y ++ ">"

instance Show a => Show (GraphG a) where
    show (GraphG nodes edges) = "GraphG: \n\t" ++
        "Nodes: " ++ show nodes ++ "\n\tEdges: " ++ show edges

instance Show a => Show (GraphE a) where
    show (GraphE edges) = "GraphE: \n\tEdges: " ++ show edges

instance Show a => Show (GraphA a) where
    show (GraphA nodecons) = "GraphA: " ++
        flatten ["\n\t" ++ show nc | nc <- nodecons]

-- -- FUNCTIONS
-- conversions
convert_gtoe :: GraphG a -> GraphE a
convert_gtoe (GraphG nodes edges) = (GraphE edges)

convert_etog :: Eq a => GraphE a -> GraphG a
convert_etog (GraphE edges) =
    let nodes = unique $ extract_nodes edges in
    GraphG nodes edges

convert_gtoa :: (Eq a, Show a) => GraphG a -> GraphA a
convert_gtoa (GraphG nodes edges) =
    GraphA [(n, [edgecomp e n | e <- edges, edgecontains e n]) | n <- nodes]

-- convert_atog :: (Eq a, Show a) => GraphA a -> GraphG a
-- convert_atog (GraphA nodecons) =
--     GraphG 

inco :: Edge a -> Node a
inco (Edge (nx, ny)) = ny
outg :: Edge a -> Node a
outg (Edge (nx, ny)) = nx

edgecomp :: (Eq a, Show a) => Edge a -> Node a -> Node a
edgecomp t n
    | n == inco t   = outg t
    | n == outg t   = inco t
    | otherwise     = -- error "tuple does not contain node"
        error ("<" ++ show t ++ "> does not contain <" ++ show n ++ ">")

edgecontains :: (Eq a, Show a) => Edge a -> Node a -> Bool
edgecontains e n
    | n == inco e   = True
    | n == outg e   = True
    | otherwise     = False

-- basic functionality
nodesG :: GraphG a -> [Node a]
nodesG (GraphG ns es) = ns

edgesG :: GraphG a -> [Edge a]
edgesG (GraphG ns es) = es

-- helpers
extract_nodes :: [Edge a] -> [Node a]
extract_nodes [] = []
extract_nodes edges = flatten $ map (\t -> [inco t, outg t]) edges

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x:(unique [y | y <- xs, y /= x])

-- -- TESTING COMPONENTS
b = Node 'b'
c = Node 'c'
d = Node 'd'
f = Node 'f'
g = Node 'g'
h = Node 'h'
k = Node 'k'
ns = [b, c, d, f, g, h, k]
es = [Edge e | e <- [(g, h), (b, c), (b, f), (c, f), (f, k)]]
gr = GraphG ns es
