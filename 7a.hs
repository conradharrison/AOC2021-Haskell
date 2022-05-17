import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List
import Debug.Trace
import Data.List.Split

updateCounts :: Int -> [(Int, Int)] -> [(Int, Int)] 
updateCounts _ [] = []
updateCounts e ((i,v):xs) = if e==i then ((i,v+1):xs) else ((i,v):(updateCounts e xs))

bIsInA :: Int -> [(Int, Int)] -> Bool
bIsInA _ [] = False
bIsInA b ((i,v):xs) = if b==i then True else (bIsInA b xs)

count :: [Int] -> [(Int, Int)]
count l = foldl (\a b -> if (bIsInA b a) then (updateCounts b a) else (a ++ [(b, 1)])) [] l

center :: [Int] -> Int
center l = floor $ (sqrt $ fromIntegral n) / (fromIntegral d)
           where (n, d) = foldl (\(s,w) (i,c) -> (s+((i*c)*(i*c)), w+c)) (0,0) l'
                          where l' = count l

cost' l k = foldl (\a (i,v) -> a + v*(abs (i-k))) 0 (count l)

nonlinearcost' :: [Int] -> Int -> Int
nonlinearcost' l k = foldl (\a (i,v) -> a + v*(f i)) 0 (count l)
                     where f x = floor $ ((fromIntegral (abs (x-k))) * (fromIntegral ((abs (x-k))+1))) / (fromIntegral 2)

cost l = map (\x->nonlinearcost' l x)  [0..(maximum l)]

mincost l = minimum (cost l)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ mincost $ map (\x->(read x::Int)) (splitOneOf "," (head (lines inStr)))
