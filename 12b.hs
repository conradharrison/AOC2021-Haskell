import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List as L
import Data.Sequence as S
import Data.List.Split
import Debug.Trace

data Graph = Graph [Node] [Edge] deriving Show
data Node = Node String Int [String] deriving Show -- name, visit_count
data Edge = Edge String String deriving Show

instance Eq Node where
    (Node a _ _) == (Node b _ _) = a==b

readInput :: [String] -> Graph
readInput lines = foldl (\g e -> addSegment g e) (Graph [] []) lines

addSegment :: Graph -> String -> Graph
addSegment (Graph ns es) s = let (a:b:_) = (splitOn "-" s) in Graph (L.union [(Node a 0 []),(Node b 0 [])] ns) (es++[Edge a b])

nameIsUpper :: String -> Bool
nameIsUpper s = s<="ZZ"
nameIsLower :: String -> Bool
nameIsLower s = s>"ZZ"
lowerTwoVisitDone :: [Node] -> Bool
lowerTwoVisitDone ns = foldl (\a (Node s c _) -> a || ((nameIsLower s)&&(c==2))) False ns

-- We *can* go here next
nextNodes :: Graph -> Node -> [Node]
nextNodes _ (Node "end" _ _) = []
nextNodes (Graph ns es) (Node s _ _) = L.filter (\(Node name count _) -> 
                                            ((isConnected es s name) && 
                                             (name/="start") && 
                                             (((nameIsLower name) && (count<1)) || 
                                              (not (lowerTwoVisitDone ns))||
                                              (nameIsUpper name)||
                                              (name=="end")))) ns

isConnected :: [Edge] -> String -> String -> Bool
isConnected es s name = foldl (\tf (Edge a b)->tf||(((a==s)&&(b==name))||((a==name)&&(b==s)))) False es

allPaths :: Graph -> (Graph, [[String]])
allPaths g = walk g (Node "start" 0 [])

walk :: Graph -> Node -> (Graph, [[String]])
walf g (Node "end" _ _) = (g, [["end"]])
walf (Graph ns es) (Node s c l) = foldl (\((Graph nss ess), x) (Node ss cc ll) -> 
                                        (let (gg, pps) = (walk (Graph (updateNodeCount ns s) es) (Node ss cc ll)) in ((Graph (updateNodeCount ns s) es), x ++ (map (\p->(s:p)) pps)))) 
                                     ((Graph (updateNodeCount ns s) es), [])
                                     (let nns =(nextNodes (Graph (updateNodeCount ns s) es) (Node s c l)) in nns)

updateNodeCount :: [Node] -> String -> [Node]
updateNodeCount [] _ = []
updateNodeCount ((Node s c l):us) v | s==v = (Node s (c+1) l):us
                                    | otherwise = (Node s c l):(updateNodeCount us v)
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ L.length $ snd $ allPaths $ readInput (lines inStr)
