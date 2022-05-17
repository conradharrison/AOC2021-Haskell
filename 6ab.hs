import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List
import Debug.Trace
import Data.List.Split

-- population is a modeled as a list of pending days untill reproduction
--

step :: [Int] -> [Int]
step []     = []
step (f:fs) = (if f==0 then 6 else f-1) : rest
            where rest = if f==0 then ((step fs)++[8]) else (step fs)

run :: [Int] -> Int -> [Int]
run f n = foldl (\a b -> step a) f [1..n]

updateCounts :: Int -> [(Int, Int)] -> [(Int, Int)] 
updateCounts _ [] = []
updateCounts e ((i,v):xs) = if e==i then ((i,v+1):xs) else ((i,v):(updateCounts e xs))

bIsInA :: Int -> [(Int, Int)] -> Bool
bIsInA _ [] = False
bIsInA b ((i,v):xs) = if b==i then True else (bIsInA b xs)

count :: [Int] -> [(Int, Int)]
count l = foldl (\a b -> if (bIsInA b a) then (updateCounts b a) else (a ++ [(b, 1)])) [] l

result l n = foldl (\a (i,v) -> a + (length (run [i] n))*v) 0 (count l)

evolveStep' :: [(Int, Int)] -> [(Int, Int)]
evolveStep' [] = []
evolveStep' ((i,v):xs) = (if i==0 then [(6, v), (8, v)] else [(i-1, v)]) ++ (evolveStep xs)

count6s :: [(Int, Int)] -> Int
count6s l = foldl (\a (i,v) -> (if i==6 then a+v else a)) 0 l

-- Since 0 and 7 both evolve to 6, compact duplicate 6'es
evolveStep l = (filter (\(i,v)->i/=6) l') ++ [(6, (count6s l'))]
               where l' = evolveStep' l

--evolve l n = foldl (\a b -> evolveStep a) (count l) [0..n]
evolve l n = foldl (\a b -> evolveStep a) (count l) [1..n]

finalPop l n = foldl (\a (i,v) -> a + v) 0 (evolve l n)

main :: IO()
main = do
        print $ finalPop (map (\x->(read x::Int)) $ splitOneOf "," "3,4,3,1,2,1,5,1,1,1,1,4,1,2,1,1,2,1,1,1,3,4,4,4,1,3,2,1,3,4,1,1,3,4,2,5,5,3,3,3,5,1,4,1,2,3,1,1,1,4,1,4,1,5,3,3,1,4,1,5,1,2,2,1,1,5,5,2,5,1,1,1,1,3,1,4,1,1,1,4,1,1,1,5,2,3,5,3,4,1,1,1,1,1,2,2,1,1,1,1,1,1,5,5,1,3,3,1,2,1,3,1,5,1,1,4,1,1,2,4,1,5,1,1,3,3,3,4,2,4,1,1,5,1,1,1,1,4,4,1,1,1,3,1,1,2,1,3,1,1,1,1,5,3,3,2,2,1,4,3,3,2,1,3,3,1,2,5,1,3,5,2,2,1,1,1,1,5,1,2,1,1,3,5,4,2,3,1,1,1,4,1,3,2,1,5,4,5,1,4,5,1,3,3,5,1,2,1,1,3,3,1,5,3,1,1,1,3,2,5,5,1,1,4,2,1,2,1,1,5,5,1,4,1,1,3,1,5,2,5,3,1,5,2,2,1,1,5,1,5,1,2,1,3,1,1,1,2,3,2,1,4,1,1,1,1,5,4,1,4,5,1,4,3,4,1,1,1,1,2,5,4,1,1,3,1,2,1,1,2,1,1,1,2,1,1,1,1,1,4") 256
