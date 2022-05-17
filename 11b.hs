import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List as L
import Debug.Trace
import Data.List.Split
import Data.Sequence as S

allFired :: Grid Int -> Bool
allFired grid = foldl (\f y -> f && (computeRow grid y)) True [0..(getHeight grid)-1]
            where computeRow grid y = foldl (\f' x -> (f' && (computeXY grid x y))) True [0..(getWidth grid)-1]
                                 where computeXY grid x y = (getXY grid x y) <= 9


allFiredTogether :: Grid Int -> Bool
allFiredTogether grid = foldl (\f y -> f && (computeRow grid y)) True [0..(getHeight grid)-1]
            where computeRow grid y = foldl (\f' x -> (f' && (computeXY grid x y))) True [0..(getWidth grid)-1]
                                 where computeXY grid x y = (getXY grid x y)==0
type Grid a = Seq (Seq a)
type Row a = Seq a

formatGrid :: Grid Int -> String
formatGrid g = unlines $ foldl (\s row -> (s ++ [(show row)])) [] g 

getWidth :: Grid a -> Int
getWidth grid = S.length (index grid 0)

getHeight :: Grid a -> Int
getHeight grid = S.length grid

getXY s x y = S.index (index s y) x
setXY s x y val = S.update y (update x val (index s y)) s

getAdjXY :: Grid Int -> Int -> Int -> [(Int, Int)]
getAdjXY grid x y = L.filter (\(x',y') -> outOfBounds x' y') (adjGen <*> [(x, y)])
                    where outOfBounds x' y' = not (x'<0 || y'<0 || x'>=width || y'>=height)
                          adjGen = [ (\(x,y)->(x+1,y+1)),
                                     (\(x,y)->(x+1,y-1)),
                                     (\(x,y)->(x+1,y  )),
                                     (\(x,y)->(x-1,y+1)),
                                     (\(x,y)->(x-1,y-1)),
                                     (\(x,y)->(x-1,y  )),
                                     (\(x,y)->(x  ,y+1)),
                                     (\(x,y)->(x  ,y-1)) ]
                          width = getWidth grid
                          height = getHeight grid

fixNegatives :: Grid Int -> Grid Int
fixNegatives grid = foldl (\g y -> g |> (computeRow grid y)) (S.empty) [0..(getHeight grid)-1]
            where computeRow grid y = foldl (\row x -> (row |> (computeXY grid x y))) (S.empty) [0..(getWidth grid)-1]
                                 where computeXY grid x y = max (getXY grid x y) 0

fireBoard :: Grid Int -> Int -> (Grid Int, Int)
fireBoard grid n = foldl (\(g, cs) y -> (let (ggg, css) = (computeRow g y) in (ggg, css+cs))) (grid, n) [0..(getHeight grid)-1]
            where computeRow g y = foldl (\(g', rs) x -> (let (gg, rss) = (computeXY g' x y) in (gg, rss+rs))) (g, 0) [0..(getWidth grid)-1]
                                 where computeXY g' x y = if (getXY g' x y)>9 then ((updateNeighbours g' x y), 1) else (g', 0)
                                                          where updateNeighbours g' x y = setXY (foldl (\g'' (px,py) -> updateNeighbour g'' px py) g' (getAdjXY g' x y)) x y (-100)
                                                                                          where updateNeighbour g'' px py = (let value = (getXY g'' px py) in (if (value/=0) then (setXY g'' px py (value+1)) else g''))

incBoard :: Grid Int -> Grid Int
incBoard grid = foldl (\g y -> g |> (computeRow grid y)) (S.empty) [0..(getHeight grid)-1]
            where computeRow grid y = foldl (\row x -> (row |> (computeXY grid x y))) (S.empty) [0..(getWidth grid)-1]
                                 where computeXY grid x y = ((getXY grid x y)+1)

fire :: Grid Int -> Int -> (Grid Int, Int)
fire g n = let g' = (fixNegatives $ fg) in (if (allFired g') then (g', fn) else (fire g' fn))
                                                      where (fg, fn) = fireBoard g n

step :: Grid Int -> Int -> (Grid Int, Int)
step grid n = fire (incBoard grid) n

run :: Grid Int -> Int -> (Grid Int, Int)
run grid n = foldl (\(g',sum) _ -> (let (fg, fn) = step g' 0 in (fg, fn+sum))) (grid, 0) [1..n]

search grid n = let aft = (allFiredTogether fg) in (if aft then (fg, n) else (search fg (n+1)))
                                                      where (fg, _) = step grid 0

makeMap :: String -> Row Int
makeMap line = fromList (map (\x->(read [x]::Int)) line)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        putStrLn $ formatGrid $ fromList (map makeMap (lines inStr))
        let (g, n) = run (fromList (map makeMap (lines inStr))) 100 in (putStrLn $ (show n ++ "\n" ++ formatGrid g))
        let (g, n) = search (fromList (map makeMap (lines inStr))) 0 in (putStrLn $ (show n ++ "\n" ++ formatGrid g))
