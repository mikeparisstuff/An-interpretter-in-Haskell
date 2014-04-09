import System.IO
import Data.List as List

type Type = String
type Name = String
type LineNo = Int

data Class = Class String [Attribute] deriving(Show)
data Attribute = Attribute Name Type (Maybe Expr) deriving(Show)
data Expr = Integer LineNo Type Int deriving (Show)


-- Read in the class map from .cl-type and create data structure
parse_cm [] = error "empty input file"
parse_cm ("class_map": num_classes : tl) = 
    let nc = read num_classes :: Int 
    in parse_cm_class nc tl

parse_cm_class _ [] = error "empty class in class map"
parse_cm_class 0 _  = []
parse_cm_class n (cname : num_attr : tl) =
    let na = read num_attr :: Int
        (attributes, rem_input) = parse_cm_attribute na tl
    in (Class cname attributes) : parse_cm_class (n-1) rem_input

parse_cm_attribute _ [] = error "empty attribute in class map"
parse_cm_attribute 0 rem_input = ([], rem_input)
parse_cm_attribute n xs = case xs of 
    ("initializer" : attr_name : attr_type : tl) -> 
        let (expr, rem_input) = parse_cm_expr tl
            (attrs, fin_rem_input) = parse_cm_attribute (n-1) rem_input
        in ((Attribute attr_name attr_type (Just expr) ) : attrs, fin_rem_input)
    ("no_initializer" : attr_name : attr_type : tl) -> 
        let (attrs, rem_input) = parse_cm_attribute (n-1) tl
        in ((Attribute attr_name attr_type Nothing) : attrs, rem_input)

parse_cm_expr xs = case xs of
    (line_no : expr_type : "integer" : val : tl) ->
        let n = read line_no :: Int
            v = read val :: Int 
        in ((Integer n expr_type v), tl)
    _ -> error "could not match class map expr"

-- Main Execution
main = do
	contents <- readFile "simple.cl-type"
	putStrLn $ show $ parse_cm $ lines contents
	--putStr contents
