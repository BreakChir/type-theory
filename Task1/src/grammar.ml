open Buffer

type expr =
    | Var  of string
    | Abst of string * expr
    | Appl of expr * expr;;

let rec string_of_expr = function
    | Var s       -> s
    | Abst (x, e) -> "(\\" ^ x ^ "." ^ string_of_expr e ^ ")"
    | Appl (a, b) -> "(" ^ string_of_expr a ^ " " ^ string_of_expr b ^ ")";;