open Grammar
open Buffer

let (>>) x f = f x;;

let buf = create 10_000_000;;

try
  while true do
    let line = stdin >> input_line in
      add_string buf line;
      add_char buf ' ';
  done;  
with End_of_file ->
  ()

let expr = buf >> contents >> Lexing.from_string >> Parser.main Lexer.main;;  

expr >> string_of_expr >> print_endline;;