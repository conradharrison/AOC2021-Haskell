import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List as L
import Debug.Trace
import Data.List.Split
import Data.Sequence as S

type Grid a = Seq (Seq a)
type Row a = Seq a

getWidth :: Grid a -> Int
getWidth grid = S.length (index grid 0)

getHeight :: Grid a -> Int
getHeight grid = S.length grid

getXY s x y = S.index (index s y) x
setXY s x y val = S.update y (update x val (index s y)) s

getBasins :: Grid Int -> (Grid Int, [Int])
getBasins s = foldl (\(s',t) y -> computeRow s' t y) (s, []) [0..(getHeight s)-1]
                   where computeRow s' t y = foldl (\(s'', c) x -> (let (s''', c')=(getBasinAtXY s'' x y) in (s''', c++[c']))) (s', t) [0..(getWidth s)-1]
                                           where getBasinAtXY s'' x y = (if ((getXY s'' x y) /= 9 && (getXY s'' x y) /= (-1)) then (getBasinsAdj s'' x y) else (s'', 0))

getBasinsAdj :: Grid Int -> Int -> Int -> (Grid Int, Int)
getBasinsAdj grid x y =  foldl 
                            (\(g, c) (x',y') -> 
                                if (getXY g x' y' == -1) then (g ,c) 
                                else let (s', c') = (getBasinsAdj g x' y') in (s', c+c'))
                            ((setXY grid x y (-1)), 1)
                            (L.filter (\(p,q) -> (getXY grid p q) /= 9) (adjs))
                         where adjs = getXYAdj grid x y

getXYAdj :: Grid Int -> Int -> Int -> [(Int, Int)]
getXYAdj s x y = case (x==0, y==0, x==(w-1), y==(h-1)) of 
                    ( True,  True,     _,     _) -> [(x+1, y), (x, y+1)]
                    ( True,     _,     _,  True) -> [(x+1, y), (x, y-1)]
                    (    _,  True,  True,     _) -> [(x-1, y), (x, y+1)]
                    (    _,     _,  True,  True) -> [(x-1, y), (x, y-1)]
                    ( True, False,     _, False) -> [(x+1, y), (x, y+1), (x, y-1)]
                    (False,  True, False,     _) -> [(x+1, y), (x, y+1), (x-1, y)]
                    (    _, False,  True, False) -> [(x-1, y), (x, y+1), (x, y-1)]
                    (False, False,     _,  True) -> [(x-1, y), (x, y-1), (x+1, y)]
                    (    _,     _,     _,     _) -> [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]
               where w = getWidth s
                     h = getHeight s

result :: Grid Int -> Int
result s = foldl (*) 1 $ L.take 3 (L.reverse $ L.sort $ sizes)
        where (_, sizes) = getBasins s 

makeMap :: String -> Row Int
makeMap line = fromList (map (\x->(read [x]::Int)) line)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ result $ fromList (map makeMap (lines inStr))
