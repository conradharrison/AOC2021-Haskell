import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Debug.Trace

bintoDec :: String -> Int
bintoDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- For each digit in the input binary string, update the count of 1s
onesCounter' :: String -> [Int] -> [Int]
onesCounter' [] [] = []
onesCounter' (b:bs) (c:cs) = [(if b=='1' then (c+1) else c)] ++ onesCounter' bs cs

-- For each binary string in input list, accumulate count of 1s for each bit position
onesCounter :: [String] -> [Int] -> [Int]
onesCounter [] c = c
onesCounter (x:xs) c = onesCounter xs $ onesCounter' x c

gammaRate :: [String] -> String
gammaRate l = map (\x->(if x>=n-x then '1' else '0')) (onesCounter l (map (\x->0) (head l)))
                where n = length l

epsilonRate :: [String] -> String
epsilonRate l = map (\x->(if x=='1' then '0' else '1')) (gammaRate l)

filterNumbers :: ([String] -> String) -> [String] -> Int -> String
filterNumbers _ (x:[]) _ = x
filterNumbers f l i = filterNumbers f fl (i+1)
                      where fl = filter (\x->(x !! i)==c) l 
                                 where c = (f l) !! i

o2genrating l = filterNumbers gammaRate l 0
co2scrubrating l = filterNumbers epsilonRate l 0

diagnose l = (bintoDec (o2genrating l)) * (bintoDec (co2scrubrating l))

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ diagnose (lines inStr)
