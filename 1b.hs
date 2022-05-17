import System.Environment ( getArgs )

ups :: [Int] -> Int
ups [] = 0
ups (x:[]) = 0
ups (x:y:[]) = 0
ups (x:y:z:[]) = 0
ups (x:y:z:rest) = if (head rest) > x then k+1 else k where k = ups (y:z:rest)

main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ ups $ map (\x->read x::Int) (lines inStr)
