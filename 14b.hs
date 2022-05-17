import System.Environment ( getArgs )
import Data.List
import Data.List.Split
import Debug.Trace
import qualified Data.Map as M

data Pair = Pair Char Char deriving Show
instance Ord Pair where
    (Pair ch1 ch2) `compare` (Pair ch3 ch4) = ([ch1]++[ch2])`compare`([ch3]++[ch4])
instance Eq Pair where
    (Pair ch1 ch2) == (Pair ch3 ch4) = ([ch1]++[ch2])==([ch3]++[ch4])
type Rules = M.Map Pair Char

type CharTable = M.Map Char Int
initCharTable :: String -> Rules -> CharTable
initCharTable template rules = M.fromList (map (\ch -> (ch, (countChars template ch))) (getAllChars rules))

countChars :: String -> Char -> Int
countChars [] _ = 0
countChars (a:rest) c = (if a==c then 1 else 0) + countChars rest c

type PairTable = M.Map Pair Int
initPairTable :: String -> Rules -> PairTable
initPairTable template rules =  let clist = getAllChars rules in M.fromList [((Pair a b),(countPairs template (Pair a b)))|a<-clist,b<-clist]

countPairs :: String -> Pair -> Int
countPairs (a:[]) _ = 0
countPairs (a:b:rest) p = (if (Pair a b)==p then 1 else 0) + countPairs (b:rest) p

getAllChars :: Rules -> [Char]
getAllChars rules = foldl (\acc (Pair a b) -> acc `union` [a,b]) "" (M.keys rules)

readInput :: [String] -> ([Char], Rules)
readInput l = let (template:rules) = (splitOn [""] l) in
                (head template, (foldl (\acc a->(let ((p:q:_):_:_:_:new:_)=(splitOneOf "-> " a) in (M.insert (Pair p q) (head new) acc))) (M.empty) (head rules)))

updateTables :: Rules -> PairTable -> CharTable -> Pair -> Int -> (PairTable, CharTable)
updateTables rules pt ct p c = let (Just val) = (M.lookup p rules) in ((M.adjust (subtract c) p (let (Pair a b)=(p) in (M.adjust (+c) (Pair a val) (M.adjust (+c) (Pair val b) pt))) ), (M.adjust (+c) val ct))

run :: PairTable -> CharTable -> Rules -> Int -> (PairTable, CharTable)
run pt ct rules level | level==0  = (pt, ct)
                      | otherwise = let (ptNew, ctNew)=(foldl (\(ptAcc, ctAcc) (p, c) -> (updateTables rules ptAcc ctAcc p c)) (pt, ct) (filter (\(p, x)->x/=0) (M.toList pt))) in (run ptNew ctNew rules (level-1))

getDiff :: CharTable -> Int
getDiff ct = let l = (M.elems ct) in (maximum l - minimum l)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ let (template, rules)=(readInput (lines inStr)) in (let (pt,ct)=(run (initPairTable template rules) (initCharTable template rules) rules 40) in (getDiff ct))
