import System.Environment ( getArgs )

ups :: [Int] -> Int
ups [] = 0
ups (x:[]) = 0
ups (x:xs) = if (head xs) > x then k+1 else k where k = ups xs 

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ ups $ map (\x->read x::Int) (lines inStr)
