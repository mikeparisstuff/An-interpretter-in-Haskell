import System.IO

main = do
	contents <- readFile "simple.cl-type"
	putStrLn $ show $ lines contents
	putStr contents


--class_map ["class_map"] ++ [] = "class_map"
