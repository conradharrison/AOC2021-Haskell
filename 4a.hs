import System.Environment ( getArgs )
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List
import Debug.Trace
import Data.List.Split

data Board = Board Int [Int] Bool deriving Show

--winner :: [Int] -> [Board] -> [Int] -> Board
--
--
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
checkBoard (Board _ [] _) = False
checkBoard (Board _ b _) = (checkRows b 5) || (checkColumns b 5)

updateBoards :: [Board] -> Int -> [Board]
updateBoards [] n = []
updateBoards ((Board s b r):rest) n = (Board s newgrid (checkBoard (Board s newgrid r))) : updateBoards rest n
                                      where newgrid = map (\x->if x==n then -1 else x) b

runBoard :: [Int] -> [Board] -> (Board, Int)
runBoard [] b = ((Board 0 [] False), 0)
runBoard (n:ns) b | done==True = ((Board 0 winner True), n)
                  | otherwise  = runBoard ns results
                    where (Board _ winner done) = if (length (winningBoards results))==0 then (Board 0 [] False) else (head (winningBoards results))
                          winningBoards x = filter (\(Board p q r) -> r) x
                          results = updateBoards b n
             
loadBoards :: [String] -> [Int] -> [Board] -> [Board]
loadBoards [] l b                 = b ++ [Board 0 l False]
loadBoards (s:ss) l b | l == []   = loadBoards ss (map (\x->(read x::Int)) (words s)) b
                      | s == ""   = loadBoards ss [] $ b ++ [Board 0 l False]
                      | otherwise = loadBoards ss (l++(map (\x->(read x::Int)) (words s))) b

loadData :: [String] -> ([Int], [Board])
loadData (s:ss) = ((map (\x->(read x::Int)) (splitOn "," s)), (loadBoards ss [] []))

run :: [String] -> Int
run l = computeAnswer $ runBoard r b 
        where (r, b) = loadData l
              computeAnswer ((Board s b r), n) = (foldl (\x y -> x + (if y /= -1 then y else 0)) 0 b) * n

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ run (lines inStr)

                    --where (Board _ winner done) = foldl ((Board _ _ r') (Board p q r) -> if r==True then(Board p q True) else (Board 0 [] False)) (Board 0 [] False) results
