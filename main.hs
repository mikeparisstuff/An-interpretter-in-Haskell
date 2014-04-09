import System.IO
import Data.List as List

data Type = Type String
type Name = String

data Class = Class String [Attribute]
data Attribute = Attribute Name Type (Maybe Expr)
type Expr = String



parse_cm [] = error "empty input file"
parse_cm ("class_map": num_classes : tl) = 
    let nc = read num_classes :: Int 
    in parse_cm_class nc tl

parse_cm_class _ [] = error "empty class"
parse_cm_class 0 _  = "Got Zero"
parse_cm_class n cls's = "Got Classes"



-- Main Execution
main = do
	contents <- readFile "simple.cl-type"
	putStrLn $ parse_cm $ lines contents
	--putStr contents
