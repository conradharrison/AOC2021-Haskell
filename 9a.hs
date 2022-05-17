import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List
import Debug.Trace
import Data.List.Split

getWidth s = length (head s)
getHeight s = length s

getXY    s x y = (s !! y) !! x
getRight s x y = getXY s (x+1)  y
getLeft  s x y = getXY s (x-1)  y
getUp    s x y = getXY s  x    (y-1)
getDown  s x y = getXY s  x    (y+1)

getAdj :: [[Int]] -> Int -> Int -> [Int]
getAdj s x y = case (x==0, y==0, x==(w-1), y==(h-1)) of 
                    ( True,  True,     _,     _) -> [getRight s x y, getDown s x y]
                    ( True,     _,     _,  True) -> [getRight s x y, getUp s x y]
                    (    _,  True,  True,     _) -> [getLeft s x y, getDown s x y]
                    (    _,     _,  True,  True) -> [getLeft s x y, getUp s x y]
                    ( True, False,     _, False) -> [getRight s x y, getDown s x y, getUp s x y]
                    (False,  True, False,     _) -> [getRight s x y, getDown s x y, getLeft s x y]
                    (    _, False,  True, False) -> [getLeft s x y, getDown s x y, getUp s x y]
                    (False, False,     _,  True) -> [getLeft s x y, getUp s x y, getRight s x y]
                    (    _,     _,     _,     _) -> [getRight s x y, getDown s x y, getLeft s x y, getUp s x y]
               where w = getWidth s
                     h = getHeight s
                
isLowPoint s x y = (getXY s x y)<(minimum (getAdj s x y))
riskLevel s x y = (getXY s x y) + 1

totalRiskLevel s = foldl (\s y -> s + rowSum y) 0 [0..(getHeight s)-1]
                   where rowSum i = foldl (\sx x -> sx + (risk x i)) 0 [0..(getWidth s)-1]
                                  where risk j k = (if (isLowPoint s j k) then (riskLevel s j k) else 0)

addPadding s =  map (\x-> [head x] ++ x ++ [last x]) ([(head s)] ++ s ++ [(last s)])

count :: [[Int]] -> Int
count s = totalRiskLevel $ s

makeMap :: String -> [Int]
makeMap line = map (\x->(read [x]::Int)) line

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ count $ map makeMap (lines inStr)
