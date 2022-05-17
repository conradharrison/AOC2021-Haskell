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
type Counts = M.Map Char Int

readInput :: [String] -> ([Char], Rules)
readInput l = let (template:rules) = (splitOn [""] l) in
                (head template, (foldl (\acc a->(let ((p:q:_):_:_:_:new:_)=(splitOneOf "-> " a) in (M.insert (Pair p q) (head new) acc))) (M.empty) (head rules)))

initCounts :: Counts
initCounts = M.fromList (map (\ch -> (ch, 0)) ['A'..'Z'])


updateCounts :: Rules -> Pair -> Int -> Counts -> Counts
updateCounts rules (Pair a b) level counts = if (level<0) then 
                                                (counts) 
                                             else (
                                                let (Just val) = (M.lookup (Pair a b) rules) in (
                                                    let counts' =  (M.adjust (+1) val counts) in (
                                                        let counts'' = (updateCounts rules (Pair a val) (level-1) counts') in (
                                                            updateCounts rules (Pair val b) (level-1) counts''))))

run :: [Char] -> Rules -> Counts -> Int -> Counts
run (a:[]) _ counts _ = counts
run (a:b:rest) rules counts level = run (b:rest) rules (updateCounts rules (Pair a b) (level-1) (M.adjust (+1) b ((M.adjust (+1) a counts)))) (trace ((show rest)++(show level)) level)

getDiff :: Counts -> Int
getDiff counts = let (n, x) = (M.foldr (\c (min, max) -> (if (c<min&&c/=0) then c else min,if (c>max) then c else max)) (999999999999999, 0) counts) in (x-n)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ let (template, rules)=(readInput (lines inStr)) in (getDiff $ run template rules initCounts 20)
                    
