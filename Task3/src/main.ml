open Printf
open Grammar
open Typeinfer

let (>>) x f = f x;;

let expr = stdin >> input_line >> Lexing.from_string >> Parser.main Lexer.main;;

let rec print_list = function
    | []      -> ()
    | x :: xs -> fprintf stdout "%s" x;
                 print_list xs;;
    
print_list (type_infer expr);;