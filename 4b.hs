import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List
import Debug.Trace
import Data.List.Split

data Board = Board [Int] Bool deriving Show

checkList :: [Int] -> Bool
checkList [] = True
checkList (x:xs) = x == -1 && checkList xs

columns l n = [[x | x <- l, let Just v = elemIndex x l in (mod v n)==c] | c <- [0..(n-1)]]

checkColumns :: [Int] -> Int -> Bool
checkColumns [] _ = False
checkColumns b n = foldl (\x y -> y||x) False $ map (\x -> checkList x) (columns b n)

checkRows :: [Int] -> Int -> Bool
checkRows [] _ = False
checkRows b n = checkList (take n b) || checkRows (drop n b) n

checkBoard :: Board -> Bool
checkBoard (Board [] _) = False
checkBoard (Board b _) = (checkRows b 5) || (checkColumns b 5)

updateBoards :: [Board] -> Int -> [Board]
updateBoards [] n = []
updateBoards ((Board b r):rest) n = (Board newgrid (checkBoard (Board newgrid r))) : updateBoards rest n
                                      where newgrid = map (\x->if x==n then -1 else x) b

-- Call list, list of boards -> a winning board, call
runBoards :: [Int] -> [Board] -> (Board, Int)
runBoards [] b = ((Board [] False), 0)
runBoards (n:ns) b | done==True = ((Board winner True), n)
                  | otherwise  = runBoards ns results
                    where (Board winner done) = if (length (pendingBoards results))==0 then (Board w True) else (Board [] False) 
                          Board w t = head (updateBoards (pendingBoards b) n)
                          pendingBoards x | length x == 0 = []
                                          | otherwise     = filter (\(Board q r) -> (not r)) x
                          results = updateBoards b n
             
loadBoards :: [String] -> [Int] -> [Board] -> [Board]
loadBoards [] l b                 = b ++ [Board l False]
loadBoards (s:ss) l b | l == []   = loadBoards ss (map (\x->(read x::Int)) (words s)) b
                      | s == ""   = loadBoards ss [] $ b ++ [Board l False]
                      | otherwise = loadBoards ss (l++(map (\x->(read x::Int)) (words s))) b

loadData :: [String] -> ([Int], [Board])
loadData (s:ss) = ((map (\x->(read x::Int)) (splitOn "," s)), (loadBoards ss [] []))

run :: [String] -> Int
run l = computeScore $ runBoards r b 
        where (r, b) = loadData l
              computeScore ((Board b r), n) = (foldl (\x y -> x + (if y /= -1 then y else 0)) 0 b) * n

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ run (lines inStr)
