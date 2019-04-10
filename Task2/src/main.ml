open Printf
open Grammar
open Reduction
open De_bruijn

let (>>) x f = f x;;

let s = stdin >> input_line;;

let expr = s >> Lexing.from_string >> Parser.main Lexer.main;;

let (expr_bruijn, fvars) = expr >> to_de_bruijn;;

let normal_bruijn = give_normal_form expr_bruijn ;;

fprintf stdout "%s\n" (string_of_bruijn normal_bruijn fvars);;