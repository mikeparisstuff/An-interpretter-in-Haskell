import System.IO
import System.Environment
import Debug.Trace
import Data.List (find)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

type Type = String
type Name = String
type FormalName = String
type MethodDefiner = String
type LineNo = Int
type MethodType = Identifier
type MethodIdentifier = Identifier
type TypeIdentifier = Identifier
type NameIdentifier = Identifier
type Child = String
type Parent = String


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
        |   Let LineNo Type [LetBinding] Expr
        |   Case LineNo Type Expr [CaseElement]
        |   Internal LineNo Type Name
    deriving (Show)

data LetBinding = LetBinding NameIdentifier TypeIdentifier (Maybe Expr) deriving(Show)
data CaseElement = CaseElement NameIdentifier TypeIdentifier Expr deriving(Show)

data Program = Program [Class] deriving(Show)

type ClassMap = [Class]
type ImpMap = [Class]
type ParentMap = [(Child, Parent)]

------- Interpreter types -------
type Location = Int

type Store = Map Location Value
type Environment = Map Name Location

-- Void algebraic data type for default Void value. Perhaps think about using a maybe for Object only
data Value = CoolBool Bool
        | CoolInt Int
        | CoolString Int String
        | Object Type (Map Name Location)
        | Void
        deriving (Show)


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
    -- Let and Case
    (line_no : expr_type : "let" : num_bindings : tl) ->
        let n = read line_no :: Int
            nb = read num_bindings :: Int
            (bindings, rem_input) = parse_binding_list nb tl
            (expr, rem_input_2) = parse_expr rem_input
        in ( (Let n expr_type bindings expr), rem_input_2 )
    (line_no : expr_type : "case" : num_elems : tl) ->
        let ln = read line_no :: Int
            ne = read num_elems :: Int
            (expr, rem_input) = parse_expr tl
            (case_elems, rem_input_2) = parse_case_elements ne rem_input
        in ( (Case ln expr_type expr case_elems), rem_input_2)
    _ -> error "could not match expr"

parse_case_elements 0 tl = ([], tl)
parse_case_elements n (var_ln : var_name : type_ln : type_name : tl) =
    let ln = read var_ln :: Int
        tln = read type_ln :: Int
        (expr, rem_input) = parse_expr tl
        (elems, rem_input_2) = parse_case_elements (n-1) rem_input
    in ( (CaseElement (Identifier ln var_name) (Identifier tln type_name) expr) : elems, rem_input_2)

parse_binding_list 0 tl = ([], tl)
parse_binding_list n ("let_binding_no_init" : var_ln : var_name : type_ln : type_name : tl) =
    let ln = read var_ln :: Int
        tln = read type_ln :: Int
        (binding_list, rem_input) = parse_binding_list (n-1) tl
    in ( (LetBinding (Identifier ln var_name) (Identifier tln type_name) Nothing) : binding_list, rem_input)
parse_binding_list n ("let_binding_init" : var_ln : var_name : type_ln : type_name : tl) =
    let ln = read var_ln :: Int
        tln = read type_ln :: Int
        (expr, rem_input) = parse_expr tl
        (binding_list, rem_input_2) = parse_binding_list (n-1) rem_input
    in ( (LetBinding (Identifier ln var_name) (Identifier tln type_name) (Just expr)) : binding_list, rem_input_2)

parse_expr_list 0 tl = ([], tl)
parse_expr_list n tl =
    let (expr, rem_input) = parse_expr tl
        (expr_list, rem_input_2) = parse_expr_list (n-1) rem_input
    in ( (expr : expr_list), rem_input_2)

parse_identifier (line_no : name : tl) =
    let n = read line_no :: Int
    in ((Identifier n name), tl)

------------------------------- Interpreter Functions ---------------------------------
impMapLookup :: ImpMap -> String -> String -> ([String], Expr)
impMapLookup imp_map class_name f =
    let (Just (Class name feats)) = find (\(Class name _) -> name == class_name) imp_map
        (Just (Method _ formals _ expr)) = find (\(Method name _ _ _) -> name == f) feats
        in
        trace ("Looking for class with name: " ++ class_name) (trace ("Looking for method with name: " ++ f) (formals, expr))

--getMeth :: ImpMap -> String -> String -> Expr
--getMeth imp_map clas_name meth_name =
--    let (Just (Class name feats)) = find (\(Class name _)  -> name == clas_name) imp_map
--        (Just (Method _ _ _ expr)) = find (\(Method name _ _ _) -> name == meth_name) feats in
--    expr

getClass :: ClassMap -> String -> Class
getClass class_map name =
    let (Just clas) = find (\(Class cname _) -> cname == name) class_map in
    clas

-- Get new integer for the location in the store
newloc :: Store -> Int
newloc store =
    Map.size store

-- Find the location given a variable name
envLookup :: Environment -> String -> Maybe Int
envLookup env name =
    Map.lookup name env

storeLookup :: Store -> Int -> Maybe Value
storeLookup store loc =
    Map.lookup loc store

default_value :: String -> Value
default_value typ = case typ of
    "Int" -> CoolInt 0
    "String" -> CoolString 0 ""
    "Bool" -> CoolBool False
    _     -> Void

checkForVoidAndCreateEnv :: Int -> Value -> [(String, Int)] -> Environment
checkForVoidAndCreateEnv line_no Void formal_locs = error ("ERROR: " ++ (show line_no) ++ ": Exception: Dispatch on void.")
checkForVoidAndCreateEnv line_no (Object _ obj_map) formal_locs =
    let formal_map = Map.fromList formal_locs
        env2 = Map.union obj_map formal_map in env2

checkForVoidAndReturnType :: Int -> Value -> String
checkForVoidAndReturnType line_no Void = error ("ERROR: " ++ (show line_no) ++ ": Exception: Dispatch on void.")
checkForVoidAndReturnType line_no (Object typ _) = typ
-- unpacks a let binding to extract an expression
-- TODO fix case of no expr
--unpackLetBind :: LetBinding -> (Name, Expr)
--unpackLetBind letbind =
--         let (LetBinding (Identifier _ name) typ maybeExpr) = letbind in
--            case maybeExpr of
--                Just expr -> (name, expr)
--                -- If no init set to default for that type
--                Nothing   -> default_value typ

interpret :: (ClassMap, ImpMap, ParentMap) -> (Value, Store, Environment) -> Expr -> (Value, Store)
interpret (class_map, imp_map, parent_map) (so, store, env) expr =
    let interpret' = interpret (class_map, imp_map, parent_map)
        threadExprs :: (Value, Store, Environment) -> [Expr] -> ([Value], Store)
        threadExprs (so, store, env) [] = ([], store)
        threadExprs (so, store, env) (expr : tl) =
            let (vi, store_n) = interpret' (so, store, env) expr
                (vals, store_last) = threadExprs (so, store_n, env) tl
                in
                    (vi : vals, store_last)
    in do

    case expr of
        (Assign _ typ (Identifier _ name) expr) ->
            let (v1, store1) = interpret' (so, store, env) expr
                (Just loc) = Map.lookup name env
                store3 = Map.insert loc v1 store1
            in
            (v1, store3)

        (IdentExpr _ typ (Identifier _ name)) -> case name of
            -- If this identifier is self then just return self object and the store
            "self" -> (so, store)
            name   ->
                let l = case (envLookup env name) of
                            Just loc -> loc
                            Nothing  -> error "Should not be looking up non-existent variable in IdentExpr"
                    val = case (storeLookup store l) of
                            Just v -> v
                            Nothing -> error "Variables should always have a value in IdentExpr"
                in
                    (val, store)
        (TrueBool _ _) ->
            (CoolBool True, store)
        (FalseBool _ _) ->
            (CoolBool False, store)
        (Integer _ typ int_val) ->
            (CoolInt int_val, store)
        (Str _ typ str) ->
            (CoolString (length str) str, store)
        (New _ typ _) ->
            let t0 = case typ of
                        "SELF_TYPE" -> let (Object x _) = so in x
                        t -> t
                (Class name feat_list) = getClass class_map t0
                orig_l = newloc store
                ls = [orig_l .. (orig_l + (length feat_list) - 1)]
                assoc_l = zip [var | (Attribute var _ _) <- feat_list] ls
                -- assoc_t holds association list from var -> type name
                assoc_t = zip [var | (Attribute var _ _) <- feat_list] [typ | (Attribute _ typ _) <- feat_list]
                v1 = Object t0 (Map.fromList assoc_l)
                -- Update store here
                lookup_type var = case (List.lookup var assoc_t) of
                    Just s -> s
                    Nothing -> error "We should not be looking up the type of a non existent variable"
                store2 = foldl (\a (var, li) -> Map.insert li (default_value (lookup_type var)) a) store assoc_l
                -- We now need to initialize the attributes with their respective expr
                ------------ SETUP ------------
                -- our goal is a set of expressions that modify attrs in scope
                init_feat_list = filter (\(Attribute _ _ e) -> case e of
                                                        (Just e) -> True
                                                        Nothing -> False
                                        ) feat_list
                iD ln name = Identifier ln name
                assign_exprs = [Assign 0 t (iD 0 name) e | (Attribute name t (Just e)) <- init_feat_list]
                env' = Map.fromList assoc_l
                -- function to pass to store
                threadStore :: (Value, Environment) -> (Value, Store) -> Expr -> (Value, Store)
                threadStore (so', env') (_, store') expr =
                        interpret' (so', store', env') expr
                threadStore' = threadStore (v1, env')
                -- Actual threading of the object store
                --------- Thread Strore --------
                (v2, store3) = foldl threadStore' (Void, store2) assign_exprs
                in
                (v1, store3)
        (DynamicDispatch line_no typ e0 (Identifier _ f) exprs) ->
            let (vals, store_n1) = threadExprs (so, store, env) exprs
                (v0, store_n2) = interpret' (so, store_n1, env) e0
                v0_typ = checkForVoidAndReturnType line_no v0
                (formals, e_n1) = impMapLookup imp_map v0_typ f
                orig_l = newloc store
                ls = [orig_l .. (orig_l + (length formals) - 1)]
                assoc_l = zip vals ls
                store_n3 = foldl (\a (val, li) -> Map.insert li val a) store_n2 assoc_l
                form_ls = zip formals ls
                env2 = checkForVoidAndCreateEnv line_no v0 form_ls
                (v_n1, s_n4) = interpret' (v0, store_n3, env2) e_n1
                in (v_n1, s_n4)
        (SelfDispatch line_no typ (Identifier _ f) exprs) ->
            let (vals, store_n1) = threadExprs (so, store, env) exprs
                (formals, e_n1) = impMapLookup imp_map typ f
                orig_l = newloc store
                ls = [orig_l .. (orig_l + (length formals) - 1)]
                assoc_l = zip vals ls
                store_n3 = foldl (\a (val, li) -> Map.insert li val a) store_n1 assoc_l
                form_ls = zip formals ls
                env2 = checkForVoidAndCreateEnv line_no so form_ls
                (v_n1, s_n4) = interpret' (so, store_n3, env2) e_n1
                in (v_n1, s_n4)
        (Plus line_no typ e1 e2) ->
            let (CoolInt i1, store2) = interpret' (so, store, env) e1
                (CoolInt i2, store3) = interpret' (so, store2, env) e2
                in
                    (CoolInt (i1 + i2), store3)
        (Minus line_no typ e1 e2) ->
            let (CoolInt i1, store2) = interpret' (so, store, env) e1
                (CoolInt i2, store3) = interpret' (so, store2, env) e2
                in
                    (CoolInt (i1 - i2), store3)
        (Times line_no typ e1 e2) ->
            let (CoolInt i1, store2) = interpret' (so, store, env) e1
                (CoolInt i2, store3) = interpret' (so, store2, env) e2
                in
                    (CoolInt (i1 * i2), store3)
        (Divide line_no typ e1 e2) ->
            let (CoolInt i1, store2) = interpret' (so, store, env) e1
                (CoolInt i2, store3) = interpret' (so, store2, env) e2
                in
                    (CoolInt (i1 `quot` i2), store3)

        --(Let _ typ [] e2) ->
        --    -- Evaluate e2
        --(Let _ typ letBinds e2) ->
        --    -- TODO does not handle multiple lets
        --    -- deals with let bindings first
        --    let (id, e1) = unpackLetBind $ head letBinds
        --    -- operational semantics
        --        (v1, store2) = interpret' (so, store, env) e1
        --        loc = newloc store2
        --        store3 = Map.insert loc v1 store2
        --        env' = Map.insert id loc env
        --        (v2, store4) = interpret' (so, store3, env') e2
        --    in
        --    -- conclusion
        --    (v2, store4)
            --(DynamicDispatch _ typ expr (Identifier _ meth_name) expr_list) ->
        _ ->
            -- This should eventually return errors
            (CoolInt 10, store)


-- Main Execution
main = do
    args <- getArgs
    if length args == 0 then
        error "Please supply the .cl-type file as the command line argument"
    else do
        contents <- readFile (args !! 0)
        let class_map = parse_cm $ lines contents
            imp_map = parse_imp_map $ lines contents
            (parent_map, ast) = parse_parent_map_and_ast $ lines contents
            _null = Object "NULL" Map.empty
            newM = New 0 "Main" (Identifier 0 "")
            mainMeth = DynamicDispatch 0 "Object" newM (Identifier 0 "main") []
            curried = (class_map, imp_map, parent_map)
            (main, store) = interpret curried (_null, Map.empty, Map.empty) mainMeth
            in do

            putStrLn $ show $ main
            -- dispatch =
            -- res = interpret curried (main, store, main) dispatch
            --putStrLn $ show $ class_map
            -- multiple putstrlns do not work for whatever reason
            --putStrLn "\nImplementation Map:"
            --putStrLn $ show $ imp_map
            --putStrLn "\nParent Map:"
            --putStrLn $ show $ parent_map
            --putStrLn "\nAnnotated AST:"
            --putStrLn $ show $ ast
--            putStrLn "\nExecution:"

