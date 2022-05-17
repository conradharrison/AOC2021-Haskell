import System.Environment ( getArgs )
import Data.List as L
import Data.Sequence as S

type Stack a = Seq a 

getScore :: Char -> Int
getScore c = case c of
                ')' -> 3
                ']' -> 57
                '}' -> 1197
                '>' -> 25137

toInt :: Char -> Int
toInt c = case c of
                '(' -> 1
                '[' -> 2
                '{' -> 3
                '<' -> 4

openerOf :: Char -> Char
openerOf c = case c of
                ')' -> '('
                ']' -> '['
                '}' -> '{'
                '>' -> '<'

isOpener :: Char -> Bool
isOpener c = (c=='(' || c=='[' || c=='{' || c=='<')

getStackScore :: Stack Char -> Int
getStackScore st = foldl (\a b -> (5*a)+(toInt b)) 0 st

scoreLine :: Stack Char -> String -> Int
scoreLine st [] = if (S.null st) then 0 else (getStackScore st)
scoreLine st (c:cs) = if (isOpener c) then (scoreLine (insertAt 0 c st) cs) else ( if ((openerOf c)==(index st 0)) then (scoreLine (deleteAt 0 st) cs) else 0)

run :: [String] -> Int
run [] = 0
run l = (L.sort incompScores) !! middle
        where incompScores = (L.filter (\x -> x/=0) (map (\a -> (scoreLine S.empty a)) l))
              middle =  floor ((fromIntegral(L.length incompScores)) / 2)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ run $ lines inStr
