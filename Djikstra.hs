module Djikstra where

import Data.List
import Debug.Trace
import qualified Data.Map as M

-- Useful type name
type Weight = Float

-- Nodes are either NULL (why?) or hold an indentifier
data Node = EMPTY | Node String
instance Show Node where
    show (Node a) = show a
    show EMPTY = show "EMPTY"
instance Ord Node where
    (Node a) `compare` (Node b) = a `compare` b
    --EMPTY `compare` (Node b) = LT
instance Eq Node where
    (Node a)==(Node b) = (a==b)
    EMPTY==EMPTY = True
    EMPTY==(Node a) = False
    (Node a)==EMPTY = False

-- Edge needs two nodes, and a weight
data Edge = Edge Node Node Weight deriving Show

-- Generic graph: (list of nodes, list of weighted edges)
data Graph = Graph [Node] [Edge] deriving Show

-- Algorithm states
type CurrentNodeCost = M.Map Node Weight
type ShortestBackHops = M.Map Node Node
type UnvisitedNodes = [Node]
type RunState = (CurrentNodeCost, ShortestBackHops, UnvisitedNodes)

-- Take Graph, starting node and init algo state
initGraph :: Graph -> Node -> RunState
initGraph (Graph ns es) start = (x, y, ns) where (x, y) = foldl (\(cnc, sbh) n -> (M.insert n (if (n==start) then 0 else 9999999999) cnc, M.insert n EMPTY sbh)) (M.empty, M.empty) ns

minWithFunc :: (Ord b, Num b) => (a -> b) -> [a] -> a
minWithFunc f l = maxItem where (maxItem, _) = foldl (\(mx, mv) x -> (if (f x)<mv then (x, (f x)) else (mx, mv))) ((head l), 9999999999) l

run :: Graph -> RunState -> Node -> RunState
run _ (cnc, sbh, []) _= (cnc, sbh, [])
run (Graph ns es) (cnc, sbh, uns) finish = 
                    let (current) = minWithFunc (\n -> (let (Just we)=(M.lookup n cnc) in we)) uns
                        reduced_uns  = filter (/= current) uns
                    in

                        (if ((current == finish) && (finish /= EMPTY))
                        then (cnc, sbh, uns)
                        else
                            (let (p,q) = foldl 
                                                (\(acccnc, accsbh) x -> 
                                                    let alt = ((let (Just wei) = (M.lookup current cnc) in wei)+(let weigh=(getEdgeWeight current x) in weigh))
                                                    in (if alt < (let (Just weig)=(M.lookup x cnc) in weig)
                                                        then (M.insert x alt acccnc, M.insert x current accsbh)
                                                        else (acccnc, accsbh)))       
                                                (cnc, sbh)
                                                (filter (isNeighbour current) reduced_uns)
                                in (run (Graph ns es) (p,q,reduced_uns) finish)))
                                    where 
                                        isNeighbour :: Node -> Node -> Bool
                                        isNeighbour s n = foldl (\acc (Edge n1 n2 _) -> acc || n1==s&&n2==n || n1==n&&n2==s) False es

                                        getEdgeWeight :: Node -> Node -> Weight
                                        getEdgeWeight n1 n2 = foldl (\h (Edge a b c) -> if (a==n1 && b==n2) then c else h) 0 es

-- return the shortest path back sequence (finish->start), and the cost to reach finish
djikstraShortestPathFromTo :: Graph -> Node -> Node -> ([Node], Weight)
djikstraShortestPathFromTo graph start finish = let (cnc, sbh, _) = run graph (initGraph graph start) finish
                                          in walk graph cnc sbh 
                                            where walk g c s = ([], (let (Just cost) = M.lookup finish c in cost))

-- return costs to reach all nodes, from start
djikstraShortestPathFrom ::  Graph -> Node -> CurrentNodeCost
djikstraShortestPathFrom graph start = let (cnc, sbh, _) = (run graph (initGraph graph start) EMPTY) in cnc
