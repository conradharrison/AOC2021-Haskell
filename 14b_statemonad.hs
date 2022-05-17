import System.Environment ( getArgs )
import Data.List
import Data.List.Split
import Debug.Trace
import qualified Data.Map as M
import Control.Monad.State

data Pair = Pair Char Char deriving Show
instance Ord Pair where
    (Pair ch1 ch2) `compare` (Pair ch3 ch4) = ([ch1]++[ch2])`compare`([ch3]++[ch4])
instance Eq Pair where
    (Pair ch1 ch2) == (Pair ch3 ch4) = ([ch1]++[ch2])==([ch3]++[ch4])

type Rules = M.Map Pair Char
type CharTable = M.Map Char Int
type PairTable = M.Map Pair Int
type PolymerState = (Rules, PairTable, CharTable)
type Executor a = State PolymerState a

-- non-monadic helpers
initCharTable :: String -> Rules -> CharTable
initCharTable template rules = M.fromList (map (\ch -> (ch, (countChars template ch))) (getAllChars rules))

countChars :: String -> Char -> Int
countChars [] _ = 0
countChars (a:rest) c = (if a==c then 1 else 0) + countChars rest c

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


initState :: [Char] -> Rules -> PolymerState
initState template rules = (rules, 
                            (initPairTable template rules),
                            (initCharTable template rules))

--state monad functions

applyRules :: Executor ()
applyRules = do
                (_, pt, _) <- get
                applyRulesOnPairs (filter (\(p, x)->x/=0) (M.toList pt))


applyRulesOnPairs :: [(Pair, Int)] -> Executor ()
applyRulesOnPairs []     = return ()
applyRulesOnPairs (p:ps) = do
                            (rules, pt, ct) <- get
                            (let (pair, count) = p in (applyOnePairOneRule pair count))
                            applyRulesOnPairs ps

-- THIS VERSION DOES NOT WORK. WHY?
--applyRules :: Executor ()
--applyRules = do 
--                (rules, pt, ct) <- get
--                (foldl (\acc (p, c) -> (execState (applyOnePairOneRule p c) acc)) 
--                       (rules, pt, ct) 
--                       (filter (\(p, x)->x/=0) (M.toList pt)))

applyOnePairOneRule :: Pair -> Int -> Executor ()
applyOnePairOneRule p c = do
                (rules, pt, ct) <- get
                let (Just val) = (M.lookup p rules) in (put (rules, (M.adjust (subtract c) p (let (Pair a b)=(p) in (M.adjust (+c) (Pair a val) (M.adjust (+c) (Pair val b) pt)))), (M.adjust (+c) val ct)))

run :: Int -> Executor ()
run 0 = return ()
run levels = do
        applyRules
        run (levels-1)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ let (_, pt, ct) = (let (template, rules)=(readInput (lines inStr))
                                  in (execState (run 40) (initState template rules))) 
                in (let l = (M.elems ct) in (maximum l - minimum l))
