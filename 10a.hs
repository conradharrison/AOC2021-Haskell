import System.Environment ( getArgs )
import Data.Sequence as S

type Stack a = Seq a 

getScore :: Char -> Int
getScore c = case c of
                ')' -> 3
                ']' -> 57
                '}' -> 1197
                '>' -> 25137

openerOf :: Char -> Char
openerOf c = case c of
                ')' -> '('
                ']' -> '['
                '}' -> '{'
                '>' -> '<'

isOpener :: Char -> Bool
isOpener c = (c=='(' || c=='[' || c=='{' || c=='<')

scoreLine :: Stack Char -> String -> Int
scoreLine st [] = 0
scoreLine st (c:cs) = if (isOpener c) then (scoreLine (insertAt 0 c st) cs) else ( if ((openerOf c)==(index st 0)) then (scoreLine (deleteAt 0 st) cs) else (getScore c)) 

run :: [String] -> Int
run [] = 0
run l = foldl (\a b -> a + (scoreLine S.empty b)) 0 l

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ run $ lines inStr
