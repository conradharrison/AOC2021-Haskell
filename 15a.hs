import System.Environment ( getArgs )
import Data.List
import Data.List.Split
import Debug.Trace
import qualified Data.Map as M
import Text.Printf

data Point = Point Int Int deriving Show
instance Ord Point where
    (Point x1 y1) `compare` (Point x2 y2) = (x1*1000 + y1) `compare` (x2*1000 + y2)
instance Eq Point where
    (Point x1 y1) == (Point x2 y2) = (x1==x2) && (y1==y2)
type Risk = Int
type Visited = Bool
type GridMap = M.Map Point (Risk, Visited)
type Grid = (GridMap, Int, Int) -- MaxX, MaxY

showGrid :: Grid -> String
showGrid (g, mx, my) = printf "%s" (foldl (\accy ay -> accy++(foldl (\accx ax -> accx++(if (let (Just (r,v))=(M.lookup (Point ax ay) g) in v) == True then "T" else "F")++".") "" ([0..(mx-1)]))++"\n") "" [0..(my-1)])

explore :: Grid -> Risk -> Point -> Risk
explore (g,mx,my) risk (Point x y) | (x==(mx-1) && y==(my-1)) = risk
                                   | otherwise                = (let ns=(getNext (g,mx,my) (Point x y)) 
                                                                 in (if ns==[] then 99999999 
                                                                     else minimum $ map (\p -> explore (visitGridPoints (g,mx,my) ns) (risk + (getRisk g p)) p) ns))

                                                                 --in (if ns==[] then 99999999 else minimum $ (let u = map (\p -> explore (visitGridPoint (g,mx,my) p) (risk + (getRisk g p)) p) (trace (show (x)++show (y)++show(ns)++">>>"++show(risk)++">>>"++show (g)) ns) in (trace (show u) u))  ))
visitGridPoint :: Grid -> Point -> Grid
visitGridPoint (g,mx,my) p = ((M.insert p ((getRisk g p), True) g), mx, my)

visitGridPoints :: Grid -> [Point] -> Grid
visitGridPoints g ps = foldl (\acc p -> visitGridPoint acc p) g ps

getRisk :: GridMap -> Point -> Risk
getRisk g p = let Just (r, v) = (M.lookup p g) in r

getNext :: Grid -> Point -> [Point]
getNext (g,mx,my) (Point x y) = filter (\(Point a b) -> (a>=0 && a<mx && b>=0 && b<my) && (let (Just (r,v))=(M.lookup (Point a b) g) in (not v))) [Point (x+1) y, Point (x-1) y, Point x (y+1), Point x (y-1)]

readInput :: [String] -> Grid
readInput lines = foldl 
                      (\(arow, mx, sy) line -> (let (arg,mx,_)=(foldl 
                                                                    (\(acol,x,y) v -> ((M.insert (Point x y) ((read [v]::Int), False) acol), (x+1), y)) 
                                                                    (arow, 0, sy)
                                                                    line) 
                                                in (arg, mx, (sy+1))))
                      (M.empty, 0, 0)
                      lines

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ explore (visitGridPoint (readInput (lines inStr)) (Point 0 0)) 0 (Point 0 0)
        --print (readInput (lines inStr))
                    
