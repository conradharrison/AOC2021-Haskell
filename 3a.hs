import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')

bintoDec :: String -> Int
bintoDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

updateCounter :: String -> [Int] -> [Int]
updateCounter [] [] = []
updateCounter (b:bs) (c:cs) = [(if b=='1' then (c+1) else c)] ++ updateCounter bs cs

count :: [String] -> [Int] -> [Int]
count [] c = c
count (x:xs) c = count xs $ updateCounter x c

gCounts :: [String] -> String
gCounts l = map (\x->(if x>n-x then '1' else '0')) (count l (map (\x->0) (head l)))
                where n = length l

eCounts :: [String] -> String
eCounts l = map (\x->(if x=='1' then '0' else '1')) (gCounts l)

diagnose l = (bintoDec $ gCounts l) * (bintoDec $ eCounts l)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ diagnose (lines inStr)
