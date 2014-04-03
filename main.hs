import System.IO

main = do
	contents <- readFile "simple.cl-type"
	putStr contents
