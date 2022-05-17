import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List
import Debug.Trace
import Data.List.Split

guessNumber :: String -> [Int]
guessNumber s = case (length s) of
                 2 -> [1]
                 3 -> [7]
                 4 -> [4]
                 5 -> [2,3,5]
                 6 -> [0,6,9]
                 7 -> [8]

getRecords :: String -> ([String], [String])
getRecords s = ((take 10 ws), (drop 11 ws))
               where ws = words s

hitdigit :: Int -> Bool
hitdigit d = (d==1 || d==7 || d==4 || d==8)

guesses2Digits :: [String] -> [Int]
guesses2Digits g = map (\z -> (head (guessNumber z))) g

count :: [([String], [String])] -> Int
count l = foldl (\a b -> a + (countperline (snd b))) 0 l
            where countperline c =  foldl (\x y -> x + (if (hitdigit y) then 1 else 0)) 0 (guesses2Digits c)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ count $ map getRecords (lines inStr)
