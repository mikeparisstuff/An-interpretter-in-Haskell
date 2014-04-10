import System.IO
import Data.List as List
import Debug.Trace

type Type = String
type Name = String
type FormalName = String
type MethodDefiner = String
type LineNo = Int
type MethodType = Identifier
type MethodIdentifier = Identifier
type TypeIdentifier = Identifier
type NameIdentifier = Identifier

data Class = Class String [Feature]
        |   AstClass LineNo String [Feature] (Maybe TypeIdentifier) deriving(Show)
data Feature = Attribute Name Type (Maybe Expr)
        |   AstAttribute NameIdentifier TypeIdentifier (Maybe Expr)
        |   Method Name [FormalName] MethodDefiner Expr
        |   AstMethod NameIdentifier [Formal] MethodType Expr
        deriving(Show)
data Formal = Formal Identifier Identifier deriving(Show)
data Identifier = Identifier LineNo Name deriving(Show)
data Expr = Integer LineNo Type Int
        |   Str LineNo Type String
        |   TrueBool LineNo Type
        |   FalseBool LineNo Type
        |   Equal LineNo Type Expr Expr
        |   LessThan LineNo Type Expr Expr
        |   LessEqual LineNo Type Expr Expr
        |   Negate LineNo Type Expr
        |   IsVoid LineNo Type Expr
        |   Not LineNo Type Expr
        |   Times LineNo Type Expr Expr
        |   Divide LineNo Type Expr Expr
        |   Plus LineNo Type Expr Expr
        |   Minus LineNo Type Expr Expr
        |   IdentExpr LineNo Type Identifier
        |   New LineNo Type Identifier
        |   Assign LineNo Type Identifier Expr
        |   While LineNo Type Expr Expr
        |   If LineNo Type Expr Expr Expr
        |   Block LineNo Type [Expr]
        |   SelfDispatch LineNo Type Identifier [Expr]
        |   DynamicDispatch LineNo Type Expr MethodIdentifier [Expr]
        |   StaticDispatch LineNo Type Expr TypeIdentifier MethodIdentifier [Expr]
        |   Internal LineNo Type Name
    deriving (Show)

data Program = Program [Class] deriving(Show)


---------------------------- Class Map ---------------------------------------------
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
        let (expr, rem_input) = parse_expr tl
            (attrs, fin_rem_input) = parse_cm_attribute (n-1) rem_input
        in ((Attribute attr_name attr_type (Just expr) ) : attrs, fin_rem_input)
    ("no_initializer" : attr_name : attr_type : tl) -> 
        let (attrs, rem_input) = parse_cm_attribute (n-1) tl
        in ((Attribute attr_name attr_type Nothing) : attrs, rem_input)

------------------------ Implementation Map -----------------------------------------
parse_imp_map [] = error "empty input file"
parse_imp_map ("implementation_map" : num_classes : tl) =
    let nc = read num_classes :: Int
    in parse_imp_class nc tl
-- We have to traverse throught the file until we find the imp_map
parse_imp_map (hd:tl) = parse_imp_map tl

parse_imp_class _ [] = error "empty class in imp map"
parse_imp_class 0 _ = []
parse_imp_class n (cname : num_methods : tl) =
    let nm = read num_methods :: Int
        (methods, rem_input) = parse_imp_methods nm tl
    in (Class cname methods) : parse_imp_class (n-1) rem_input

parse_imp_methods _ [] = error "empty method in implementation map"
parse_imp_methods 0 rem_input = ([], rem_input)
parse_imp_methods n xs = case xs of
    (meth_name : num_formals : tl) -> 
        let nf = read num_formals :: Int
            (formals, rem_input) = parse_formals_list nf tl
            (meth_owner : rem_input_2) = rem_input
            (expr, rem_input_3) = parse_expr rem_input_2
            (methods, fin_rem_input) = parse_imp_methods (n-1) rem_input_3
        in ((Method meth_name formals meth_owner expr) : methods, fin_rem_input)

parse_formals_list 0 xs = ([], xs)
parse_formals_list n xs = case xs of
    (formal_name : tl) -> 
        let (formals, rem_input) = parse_formals_list (n-1) tl
        in (formal_name : formals, rem_input)
    x -> error "this should never happen in parse formals"

--------------------------- Parent Map -------------------------------------------

-- Makes and association list in the form [(subclass, superclass)]
parse_parent_map_and_ast [] = error "empty input file"
parse_parent_map_and_ast ("parent_map" : num_relations : tl) =
    let num_rel = read num_relations :: Int
        (relations, rem_lines) = parse_parent_map_relations num_rel tl
        ast = parse_ast rem_lines
    in (relations, ast)
parse_parent_map_and_ast (hd:tl) = parse_parent_map_and_ast tl

parse_parent_map_relations 0 left = ([], left)
parse_parent_map_relations n (child:parent:tl) = 
    let (relations, rem_lines) = parse_parent_map_relations (n-1) tl
    in ((child, parent) : relations, rem_lines)
parse_parent_map_relations _ _ = error "this should not happen in parent map relations"

-------------------------- Annotated Abstract Syntax tree ----------------------------

parse_ast [] = error "annotated ast not in input"
parse_ast (num_classes : tl) =
    let n = read num_classes :: Int
        classes = parse_ast_classes n tl
    in Program classes

parse_ast_classes 0 _ = []
parse_ast_classes n (line_no : cname : "inherits" : parent_line_no : parent_name : num_feats : tl) = 
    let ln = read line_no :: Int
        pln = read parent_line_no :: Int
        nf = read num_feats :: Int
        (features, rem_lines) = parse_ast_features nf tl
        classes = parse_ast_classes (n-1) rem_lines
    in ( (AstClass ln cname features (Just (Identifier pln parent_name))) : classes )
parse_ast_classes n (line_no : cname : "no_inherits" : num_feats : tl) =
    let ln = read line_no :: Int
        nf = read num_feats :: Int
        (features, rem_lines) = parse_ast_features nf tl
        classes = parse_ast_classes (n-1) rem_lines
    in ( (AstClass ln cname features Nothing) : classes)

parse_ast_features 0 last = ([], last)
parse_ast_features n ("attribute_no_init" : line_no : fname : type_line_no : type_name : tl) = 
    let ln = read line_no :: Int
        tln = read type_line_no :: Int
        (features, rem_lines) = parse_ast_features (n-1) tl
    in ((AstAttribute (Identifier ln fname) (Identifier tln type_name) Nothing ) : features, rem_lines)
parse_ast_features n ("attribute_init" : line_no : fname : type_line_no : type_name : tl) = 
    let ln = read line_no :: Int
        tln = read type_line_no :: Int
        (expr, rem_lines) = parse_expr tl
        (features, rem_lines_2) = parse_ast_features (n-1) rem_lines
    in ((AstAttribute (Identifier ln fname) (Identifier tln type_name) (Just expr)) : features, rem_lines_2)
parse_ast_features n ("method" : line_no : fname : num_formals : tl) =
    let ln = read line_no :: Int
        nf = read num_formals :: Int
        (formals, rem_lines) = parse_formal_identifiers nf tl
        (type_line_no : type_name : rem_lines_2) = rem_lines
        tln = read type_line_no :: Int
        (expr, rem_lines_3) = parse_expr rem_lines_2
        (features, rem_lines_4) = parse_ast_features (n-1) rem_lines_3
    in ((AstMethod (Identifier ln fname) formals (Identifier tln type_name) expr) : features, rem_lines_4)

parse_formal_identifiers 0 last = ([], last)
parse_formal_identifiers n (line_no : fname : type_line_no : type_name : tl) =
    let ln = read line_no :: Int
        tln = read type_line_no :: Int
        (formals, rem_lines) = parse_formal_identifiers (n-1) tl
    in ( (Formal (Identifier ln fname) (Identifier tln type_name)) : formals, rem_lines) 
------------------------ Expressions -------------------------------------------------

parse_expr xs = case xs of
    -- Integer, String, and Bool
    (line_no : expr_type : "integer" : val : tl) ->
        let n = read line_no :: Int
            v = read val :: Int 
        in ((Integer n expr_type v), tl)
    (line_no : expr_type : "string" : val : tl) ->
        let n = read line_no :: Int
        in ((Str n expr_type val), tl)
    (line_no : expr_type : "true" : tl) ->
        let n = read line_no :: Int
        in ((TrueBool n expr_type), tl)
    (line_no : expr_type : "false" : tl) ->
        let n = read line_no :: Int
        in ((FalseBool n expr_type), tl)
    -- LT, LE, EQ
    (line_no : expr_type : "eq" : tl) ->
        let n = read line_no :: Int
            (expr1, rem_input_1) = parse_expr tl
            (expr2, rem_input_2) = parse_expr rem_input_1
        in ((Equal n expr_type expr1 expr2), rem_input_2)
    (line_no : expr_type : "lt" : tl) ->
        let n = read line_no :: Int
            (expr1, rem_input_1) = parse_expr tl
            (expr2, rem_input_2) = parse_expr rem_input_1
        in ((LessThan n expr_type expr1 expr2), rem_input_2)
    (line_no : expr_type : "le" : tl) ->
        let n = read line_no :: Int
            (expr1, rem_input_1) = parse_expr tl
            (expr2, rem_input_2) = parse_expr rem_input_1
        in ((LessEqual n expr_type expr1 expr2), rem_input_2)
    -- Not, Negate, IsVoid
    (line_no : expr_type : "negate" : tl) ->
        let n = read line_no :: Int
            (expr, rem_input) = parse_expr tl
        in ((Negate n expr_type expr), rem_input)
    (line_no : expr_type : "not" : tl) ->
        let n = read line_no :: Int
            (expr, rem_input) = parse_expr tl
        in ((Not n expr_type expr), rem_input)
    (line_no : expr_type : "isvoid" : tl) ->
        let n = read line_no :: Int
            (expr, rem_input) = parse_expr tl
        in ((IsVoid n expr_type expr), rem_input)
    -- Plus, Minus, Times, Divide
    (line_no : expr_type : "times" : tl) ->
        let n = read line_no :: Int
            (expr1, rem_input_1) = parse_expr tl
            (expr2, rem_input_2) = parse_expr rem_input_1
        in ((Times n expr_type expr1 expr2), rem_input_2)
    (line_no : expr_type : "divide" : tl) ->
        let n = read line_no :: Int
            (expr1, rem_input_1) = parse_expr tl
            (expr2, rem_input_2) = parse_expr rem_input_1
        in ((Divide n expr_type expr1 expr2), rem_input_2)
    (line_no : expr_type : "plus" : tl) ->
        let n = read line_no :: Int
            (expr1, rem_input_1) = parse_expr tl
            (expr2, rem_input_2) = parse_expr rem_input_1
        in ((Plus n expr_type expr1 expr2), rem_input_2)
    (line_no : expr_type : "minus" : tl) ->
        let n = read line_no :: Int
            (expr1, rem_input_1) = parse_expr tl
            (expr2, rem_input_2) = parse_expr rem_input_1
        in ((Minus n expr_type expr1 expr2), rem_input_2)
    -- Identifier, New, Assign
    (line_no : expr_type : "identifier" : tl) ->
        let n = read line_no :: Int
            (ident, rem_input) = parse_identifier tl
        in ((IdentExpr n expr_type ident), rem_input)
    (line_no : expr_type : "new" : tl) ->
        let n = read line_no :: Int
            (ident, rem_input) = parse_identifier tl
        in ((New n expr_type ident), rem_input)
    (line_no : expr_type : "assign" : tl) ->
        let n = read line_no :: Int
            (ident, rem_input_1) = parse_identifier tl
            (expr, rem_input_2) = parse_expr rem_input_1
        in ((Assign n expr_type ident expr), rem_input_2)
    -- While, If
    (line_no : expr_type : "while" : tl) ->
        let n = read line_no :: Int
            (expr1, rem_input_1) = parse_expr tl
            (expr2, rem_input_2) = parse_expr rem_input_1
        in ((While n expr_type expr1 expr2), rem_input_2)
    (line_no : expr_type : "if" : tl) ->
        let n = read line_no :: Int
            (expr1, rem_input_1) = parse_expr tl
            (expr2, rem_input_2) = parse_expr rem_input_1
            (expr3, rem_input_3) = parse_expr rem_input_2
        in ((If n expr_type expr1 expr2 expr3), rem_input_3)
    -- Block
    (line_no : expr_type : "block" : num_exprs : tl) ->
        let n = read line_no :: Int
            n_exprs = read num_exprs :: Int
            (expr_list, rem_input) = parse_expr_list n_exprs tl
        in ((Block n expr_type expr_list), rem_input)
    -- Dispatch
    (line_no : expr_type : "self_dispatch" : tl) ->
        let n = read line_no :: Int
            (ident, rem_input) = parse_identifier tl
            (num_exprs : rem_input_1) = rem_input
            n_exprs = read num_exprs :: Int
            (expr_list, rem_input_2) = parse_expr_list n_exprs rem_input_1
        in ((SelfDispatch n expr_type ident expr_list), rem_input_2)
    (line_no : expr_type : "dynamic_dispatch" : tl) ->
        let n = read line_no :: Int
            (expr, rem_input_1) = parse_expr tl
            (ident, rem_input_2) = parse_identifier rem_input_1
            (num_exprs : rem_input_3) = rem_input_2
            n_exprs = read num_exprs :: Int
            (expr_list, rem_input_4) = parse_expr_list n_exprs rem_input_3
        in ((DynamicDispatch n expr_type expr ident expr_list), rem_input_4)
    (line_no : expr_type : "static_dispatch" : tl) ->
        let n = read line_no :: Int
            (expr, rem_input_1) = parse_expr tl
            (ident1, rem_input_2) = parse_identifier rem_input_1
            (ident2, rem_input_3) = parse_identifier rem_input_2
            (num_exprs : rem_input_4) = rem_input_3
            n_exprs = read num_exprs :: Int
            (expr_list, rem_input_5) = parse_expr_list n_exprs rem_input_4
        in ((StaticDispatch n expr_type expr ident1 ident2 expr_list), rem_input_5)
    -- Internal
    (line_no : expr_type : "internal" : class_dot_method : tl) ->
        let n = read line_no :: Int
        in ((Internal n expr_type class_dot_method), tl)
    _ -> error "could not match expr"

parse_expr_list 0 tl = ([], tl)
parse_expr_list n tl =
    let (expr, rem_input) = parse_expr tl
        (expr_list, rem_input_2) = parse_expr_list (n-1) rem_input
    in ( (expr : expr_list), rem_input_2)

parse_identifier (line_no : name : tl) = 
    let n = read line_no :: Int
    in ((Identifier n name), tl)

-- Main Execution
main = do
    contents <- readFile "simple.cl-type"
    let class_map = parse_cm $ lines contents
        imp_map = parse_imp_map $ lines contents
        (parent_map, ast) = parse_parent_map_and_ast $ lines contents
    putStrLn "Class Map:"
    putStrLn $ show $ class_map
    putStrLn "Implementation Map:"
    putStrLn $ show $ imp_map
    putStrLn "Parent Map:"
    putStrLn $ show $ parent_map
    putStrLn "Annotated AST:"
    putStrLn $ show $ ast
	--putStr contents
