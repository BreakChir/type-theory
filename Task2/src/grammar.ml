open Buffer

module H = Hashtbl

type expr =
    | Var  of string
    | Abst of string * expr
    | Appl of expr * expr;;
    
type de_bruijn =
    | DVar  of int
    | DAbst of de_bruijn
    | DAppl of de_bruijn * de_bruijn;;

let string_of_expr expr =
    let buf = create 1000 in
    let rec str_o_e = function
        | Var s       -> add_string buf s
        | Abst (x, e) -> add_string buf "(\\";
                         add_string buf x;
                         add_char buf '.';
                         str_o_e e;
                         add_char buf ')'
        | Appl (a, b) -> add_char buf '(';
                         str_o_e a;
                         add_char buf ' ';
                         str_o_e b;
                         add_char buf ')'
    in str_o_e expr;    
    contents buf;;
    
let string_of_bruijn expr int_to_var =
    let buf     = create 1000 in
    let lvl_var = (H.create 1000 : (int, string) H.t) in
    let numVar = ref 0 in

    let get_new_var () =
        numVar := !numVar + 1;
        "t" ^ (string_of_int !numVar)
    in
    
    let rec str_o_e lvl = function
        | DVar n       -> let var = if (n < 0) then H.find int_to_var n else H.find lvl_var (lvl - n) in
                          add_string buf var
        | DAbst e      -> add_string buf "(\\";
                          let var' = get_new_var () in
                          H.add lvl_var (lvl + 1) var';
                          add_string buf var';
                          add_char buf '.';
                          str_o_e (lvl + 1) e;
                          H.remove lvl_var (lvl + 1);
                          add_char buf ')'
        | DAppl (a, b) -> add_char buf '(';
                          str_o_e lvl a;
                          add_char buf ' ';
                          str_o_e lvl b;
                          add_char buf ')'
    in str_o_e 0 expr;    
    contents buf;;