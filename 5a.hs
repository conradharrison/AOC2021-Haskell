import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List
import Debug.Trace
import Data.List.Split

data Point = Point Int Int deriving Show
data Segment = Segment Point Point deriving Show

liesOnLine :: Point -> Segment -> Bool
liesOnLine (Point x y) (Segment (Point x1 y1) (Point x2 y2)) = (x1==x2 && x==x1 && (((y-y1)*(y-y2))<=0)) || (y1==y2 && y==y1 && (((x-x1)*(x-x2))<=0))

passCount :: Point -> [Segment] -> Int
passCount p s = foldl (\a b -> if (liesOnLine p b) then a+1 else a) 0 s

filterHVSegments :: [Segment] -> [Segment]
filterHVSegments s = filter (\(Segment (Point x1 y1) (Point x2 y2))->if x1==x2 || y1==y2 then True else False) s

readSegments :: [String] -> [Segment]
readSegments []     = []
readSegments (x:xs) = (Segment (Point x1 y1) (Point x2 y2)) : (readSegments xs)
                      where (x1:y1:_:x2:y2:_) = map (\x->(read x::Int)) (splitOneOf ", " x)

gridMax :: [Segment] -> Point
gridMax s = foldl (\(Point mx my) (Segment (Point x1 y1) (Point x2 y2)) -> Point (maximum [x1,x2,mx]) (maximum [y1,y2,mx])) (Point 0 0) s

getPassCounts :: [Segment] -> [[Int]]
getPassCounts s = map (\y-> (map (\x->(passCount (Point x y) s)) [0..mx])) [0..my]
                  where Point mx my = gridMax s

result :: [Segment] -> Int
result s = foldl (\p row -> p+(foldl (\a b -> if b>=2 then a+1 else a) 0 row)) 0 (getPassCounts s)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ result $ filterHVSegments $ readSegments $ lines inStr
