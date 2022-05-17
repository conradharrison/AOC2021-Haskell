import System.Environment ( getArgs )
import Data.List
import Data.List.Split
import Debug.Trace

data Rule = Rule Char Char Char deriving Show -- old pair, new value

readInput :: [String] -> ([Char], [Rule])
readInput l = let (template:rules) = (splitOn [""] l) in
                (head template, (map (\a->(let ((p:q:_):_:_:_:new:_)=(splitOneOf "-> " a) in (Rule p q (head new)))) (head rules)))

run :: [Char] -> [Rule] -> Int -> [Char]
run t rules steps = foldl (\acc x -> (applyRules acc rules)) t [1..steps]

applyRules :: [Char] -> [Rule] -> [Char]
applyRules (a:[]) _ = [a]
applyRules (a:b:rest) rules = [a, (getSub a b rules)] ++ (applyRules (b:rest) rules)

getSub :: Char -> Char -> [Rule] -> Char
getSub a b rules = foldl (\acc (Rule x y z) -> if x==a && y==b then z else acc) 'x' rules

getCounts :: [Char] -> [Int]
getCounts cs = map (\c -> (length (filter (==c) cs))) ['A'..'Z']

getDiff :: [Int] -> Int
getDiff counts = let (n, x) = (foldl (\(min, max) c -> (if (c<min&&c/=0) then c else min,if (c>max) then c else max)) (999999999999999, 0) counts) in (x-n)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ let (template, rules)=(readInput (lines inStr)) in getDiff $ (getCounts $ run template rules 10)
                    
