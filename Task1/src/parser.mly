%{
  open Grammar;;
%}
%token <string> VAR
%token ABST
%token OPEN CLOSE
%token EOF
%token DOT
%nonassoc ABST
%start main
%type <Grammar.expr> main
%%
main:
    apply EOF           { $1 }

apply:
    apply atom          { Appl ($1, $2) }
    |atom               { $1 }

atom:
    VAR                 { Var ($1) }
    |OPEN apply CLOSE   { $2 }
    |ABST VAR DOT apply { Abst ($2, $4) }