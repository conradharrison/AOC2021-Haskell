import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List
import Debug.Trace
import Data.List.Split

unionStrings :: String -> String -> String
unionStrings a b = foldl (\x y -> x ++ (if (elem y a) then "" else [y])) a b

getItemsWithNOccurances :: String -> Int -> String
getItemsWithNOccurances s n = foldl (\a b -> (if (((length (filter (==b) s)))==n && (not (elem b a))) then a++[b] else a)) [] s

matchDigits :: String -> String -> Bool
matchDigits a b = (sort a) == (sort b)

------------------------
-- Deduce digit patterns
------------------------
--   a
--  b c
--   d
--  e f
--   g
------------------------

oneString s = head $ filter (\x-> ((length x)==2)) s
fourString s = head $ filter (\x-> ((length x)==4)) s
sevenString s = head $ filter (\x-> ((length x)==3)) s
eightString s = head $ filter (\x-> ((length x)==7)) s
all069Strings s = filter (\x-> ((length x)==6)) s
all235Strings s = filter (\x-> ((length x)==5)) s

add069Strings s = foldl (++) "" (all069Strings s)
add235Strings s = foldl (++) "" (all235Strings s)
adgString s = getItemsWithNOccurances (add235Strings s) 3 
cfString s = getItemsWithNOccurances (add235Strings s) 2 
beString s = getItemsWithNOccurances (add235Strings s) 1 
cdeString s = getItemsWithNOccurances (add069Strings s) 2 
abfgString s = getItemsWithNOccurances (add069Strings s) 3 
twoString s = unionStrings (adgString s) (cdeString s)
threeString s = (adgString s) ++ (cfString s)
fiveString s = unionStrings (adgString s) (abfgString s)
sixString s = unionStrings (fiveString s) (beString s)
nineString s = unionStrings (threeString s) (fourString s)
zeroString s = unionStrings (abfgString s) (unionStrings (cfString s) (beString s))

numberPatterns l = [(zeroString l, '0'), (oneString l, '1'), (twoString l, '2'), (threeString l, '3'), (fourString l, '4'), (fiveString l, '5'), (sixString l, '6'), (sevenString l, '7'), (eightString l, '8'), (nineString l , '9')]

getNumber :: [String] -> String -> Char
getNumber l s = snd $ head $ filter (\(x,y) -> (matchDigits x s)) (numberPatterns l)

count :: [([String], [String])] -> Int
count l = foldl (\a b -> a + (countperline (b))) 0 l
            where countperline c =  read (foldl (\x y -> x ++ [(getNumber (fst c) y)]) "" (snd c)) :: Int

makeRecords :: String -> ([String], [String])
makeRecords s = ((take 10 ws), (drop 11 ws))
               where ws = words s

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ count $ map makeRecords (lines inStr)
