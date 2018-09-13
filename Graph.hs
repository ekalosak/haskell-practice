module Graph (GraphG, GraphE, GraphA, Node, unique) where

-- -- DATA CONSTRUCTORS
data Node a = Node a deriving Eq
-- graph-term form
data GraphG a = GraphG [Node a] [(Node a, Node a)] deriving Eq
-- edge-term form
data GraphE a = GraphE [(Node a, Node a)] deriving Eq
-- adjacency form
data GraphA a = GraphA [(Node a, [Node a])] deriving Eq
-- TODO: convert between GraphA and the other two
-- TODO: validation functions for these forms
-- TODO: instance show (GraphE, GraphA)

instance Show a => Show (Node a) where
    show (Node x) = show x

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
    GraphA [(n, [tuplecomp e n | e <- edges, tuplein e n]) | n <- nodes]

tuplecomp :: (Eq a, Show a) => (Node a, Node a) -> Node a -> Node a
tuplecomp t n
    | n == fst t    = snd t
    | n == snd t    = fst t
    | otherwise     = -- error "tuple does not contain node"
        error ("<" ++ show t ++ "> does not contain <" ++ show n ++ ">")

-- <tuplein t n> is true if <n> is in <t>
tuplein :: Eq a => (Node a, Node a) -> Node a -> Bool
tuplein t n
    | n == fst t    = True
    | n == snd t    = True
    | otherwise     = False

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
b = Node 'b'
c = Node 'c'
d = Node 'd'
f = Node 'f'
g = Node 'g'
h = Node 'h'
k = Node 'k'
ns = [b, c, d, f, g, h, k]
es = [(g, h), (b, c), (b, f), (c, f), (f, k)]
gr = GraphG ns es
