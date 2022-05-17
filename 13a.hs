import System.Environment ( getArgs )
import Data.List
import Data.List.Split

data State = State [(Int, Int)] [(String, Int)] deriving Show -- [(x,y),...] [("x", val), ...]

data Fold = Fold String Int deriving Show
data Point = Point Int Int deriving Show
instance Eq Point where
    (Point x1 y1) == (Point x2 y2) = (x1==x2)&&(y1==y2)

readInput :: [String] -> ([Point], [Fold])
readInput l = let (xys:folds:_) = (splitOn [""] l) in
                ((map (\a->(let (x:y:_)=(splitOn "," a) in (Point (read x::Int) (read y::Int)))) xys),
                 (map (\a->(let (fol:alon:dir:val:_)=(splitOneOf "= " a) in (Fold dir (read val::Int)))) folds))

foldAll :: [Point] -> [Fold] -> [Point]
foldAll ps [] = ps
foldAll ps (f:fs) = foldAll (foldOne ps f) fs

foldOne :: [Point] -> Fold -> [Point]
foldOne ps (Fold dir val) | dir=="x" = foldX ps val
                          | dir=="y" = foldY ps val

foldX :: [Point] -> Int -> [Point]
foldX ps pivot = foldl (\acc (Point x y) -> acc `union` (processFoldX x y pivot)) [] ps
                    where processFoldX x y pivot | x<pivot = [Point x y]
                                                 | x>pivot = [Point (pivot - (x - pivot)) y]
                                                 | otherwise = []
foldY :: [Point] -> Int -> [Point]
foldY ps pivot = foldl (\acc (Point x y) -> acc `union` (processFoldY x y pivot)) [] ps
                    where processFoldY x y pivot | y<pivot = [Point x y]
                                                 | y>pivot = [Point x (pivot - (y - pivot))]
                                                 | otherwise = []

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ (let (points, folds) = readInput (lines inStr) in foldAll points [head folds])
                    
