module Graph (GraphG, GraphE, GraphA, Node, unique) where

-- -- DATA CONSTRUCTORS
data Node a = Node a deriving Eq
data Edge a = Edge (Node a, Node a)
data Arc a = Arc (Node a, Node a, Int) deriving Eq
data Path a = Path [Node a] deriving (Show, Eq)
-- graph-term form
data GraphG a = GraphG [Node a] [Edge a] deriving Eq
-- edge-term form
data GraphE a = GraphE [Edge a] deriving Eq
-- adjacency form
data GraphA a = GraphA [(Node a, [Node a])] deriving Eq
data GraphC a = GraphC [Node a] [Arc a] deriving Eq

-- TODO: validation functions for these forms

instance Show a => Show (Node a) where
    show (Node x) = show x

instance Show a => Show (Edge a) where
    show (Edge (Node x, Node y)) = "<" ++ show x ++ "," ++ show y ++ ">"
instance Eq a => Eq (Edge a) where
    (==) x y = if ((inco x == inco y
            && outg x == outg y) ||
        (inco x == outg y
            && outg x == inco y)) then True else False

instance Show a => Show (Arc a) where
    show (Arc (Node x, Node y, d)) =
        "<" ++ show x ++ "," ++ show y ++ "," ++ show d ++ ">"


instance Show a => Show (GraphC a) where
    show (GraphC nodes arcs) = "GraphC: \n\t" ++
        "Nodes: " ++ show nodes ++ "\n\tArcs: " ++ show arcs

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

convert_atog :: (Eq a, Show a) => GraphA a -> GraphG a
convert_atog ga = GraphG (nodesA ga) (edgesA ga)

convert_etoa :: (Eq a, Show a) => GraphE a -> GraphA a
convert_etoa = convert_gtoa . convert_etog
convert_atoe :: (Eq a, Show a) => GraphA a -> GraphE a
convert_atoe = convert_gtoe . convert_atog

inco :: Edge a -> Node a
inco (Edge (nx, ny)) = ny
outg :: Edge a -> Node a
outg (Edge (nx, ny)) = nx

edgecomp :: (Eq a, Show a) => Edge a -> Node a -> Node a
edgecomp e n
    | n == inco e   = outg e
    | n == outg e   = inco e
    | otherwise     = -- error "tuple does not contain node"
        error ("<" ++ show e ++ "> does not contain <" ++ show n ++ ">")

edgecontains :: (Eq a) => Edge a -> Node a -> Bool
edgecontains e n
    | n == inco e   = True
    | n == outg e   = True
    | otherwise     = False

-- basic functionality
nodesG :: GraphG a -> [Node a]
nodesG (GraphG ns es) = ns
edgesG :: GraphG a -> [Edge a]
edgesG (GraphG ns es) = es

nodeconsA :: GraphA a -> [(Node a, [Node a])]
nodeconsA (GraphA ncs) = ncs
nodesA :: GraphA a -> [Node a]
nodesA (GraphA ncs) = [fst x | x <- ncs]
edgesA :: Eq a => GraphA a -> [Edge a]
edgesA (GraphA ncs) = uniqueEdgesA $ flatten
    [[Edge (fst nc, ne) | ne <- snd nc] | nc <- ncs]
edgesA_nonEq :: GraphA a -> [Edge a]
edgesA_nonEq = edgeAhelper

-- calculate paths in a GraphG
paths :: Node a -> Node a -> GraphG a -> [Path a]
-- paths = error "Not implemented"
paths n1 n2 (GraphG nodes edges) = let pps = initpospath b gg in
    -- TODO 

-- pospath [[a] | a <- getadjnodes b gg] gg

initpospath :: (Eq a, Show a) => Node a -> GraphG a -> [[Node a]]
initpospath n g = [[a, n] | a <- getadjnodes n g]

getadjnodes :: (Eq a, Show a) => Node a -> GraphG a -> [Node a]
getadjnodes n g = [edgecomp e n | e <- edgesG g, edgecontains e n]

pospath :: (Eq a, Show a) => [[Node a]] -> GraphG a -> [[Node a]]
pospath pps g = [n:pp | pp <- pps,
                        n <- getadjnodes (head pp) g,
                        not (elem n pp)]

-- <snd result> are complete paths, <fst result> are not yet complete
complpathssplit :: (Eq a, Show a) => [[Node a]] -> Node a ->
                                    ([[Node a]], [[Node a]])
complpathssplit pps n = ([pp | pp <- pps, (head pp) == n],
                        [pp | pp <-pps, (head pp) /= n])


-- helpers
edgeAhelper :: GraphA a -> [Edge a]
edgeAhelper (GraphA ncs) = flatten
    [[Edge (fst nc, ne) | ne <- snd nc] | nc <- ncs]

uniqueEdgesA :: Eq a => [Edge a] -> [Edge a]
uniqueEdgesA [] = []
uniqueEdgesA (e:es) =
    if (elem e es) then ues
    else e:ues
    where ues = uniqueEdgesA es

extract_nodes :: Eq a => [Edge a] -> [Node a]
-- extract_nodes [] = []
extract_nodes = unique . flatten . map (\t -> [inco t, outg t])

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
e = es !! 1
gg = GraphG ns es
ga = convert_gtoa gg
ge = convert_gtoe gg

m = Node 'm'
q = Node 'q'
p = Node 'p'
nsc = [m, p, q, k]
arcs = [Arc a | a <- [(p, m, 5), (m, q, 7), (p, q, 9)]]
gc = GraphC nsc arcs
