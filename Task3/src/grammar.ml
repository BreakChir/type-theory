open Buffer

type expr =
    | Var  of string
    | Abst of string * expr
    | Appl of expr * expr;;
    
type expr_type = 
    | Impl of expr_type * expr_type
    | VarI of string;;
    
type expr_unif = Eq of expr_type * expr_type;;
    
let (@=) a b = Eq (a, b);;
let (-->) a b = Impl (a, b);; 

let rec string_of_type expr =
    | VarI s      -> s
    | Impl (a, b) -> "(" ^ (string_of_type a) ^ " -> " ^ (string_of_type b) ^ ")";;

let rec string_of_expr expr =
    | Var s       -> s
    | Abst (x, e) -> "(\\" ^ x ^ ". " ^ (string_of_expr e) ^ ")"
    | Appl (a, b) -> "(" ^ (string_of_expr a) ^ " " ^ (string_of_expr b) ^ ")";;