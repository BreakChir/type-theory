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
      expr EOF { $1 } 

expr:
      apply ABST VAR DOT expr { Appl ($1, Abst ($3, $5)) }
    | ABST VAR DOT expr       { Abst ($2, $4) }
    | apply                   { $1 }
                              
apply:                        
      apply atom              { Appl ($1, $2) }
    | atom                    { $1 }
                              
atom:                         
      VAR                     { Var ($1) }
    | OPEN expr CLOSE         { $2 }