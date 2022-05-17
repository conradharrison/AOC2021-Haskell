import System.Environment ( getArgs )

data Command = FORWARD | DOWN | UP deriving Show
data Move = Move Command Int deriving Show
data Position = Position Int Int Int deriving Show -- x, depth, aim

toCommand :: String -> Command
toCommand s = case s of "forward" -> FORWARD
                        "down" -> DOWN
                        "up" -> UP

toMove :: String -> Move
toMove s = Move (toCommand first) (read second :: Int) where (first:second:rest) = words s

makeMove :: Move -> Position -> Position
makeMove (Move c x) (Position h d a) = case c of FORWARD -> Position (h+x) (d+a*x) a
                                                 DOWN -> Position h d (a+x)
                                                 UP -> Position h d (a-x)

navigate :: [Move] -> Position -> Position
navigate [] p = p
navigate (m:ms) p = navigate ms (makeMove m p)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ let Position h d a = navigate (map toMove (lines inStr)) (Position 0 0 0) in h*d
