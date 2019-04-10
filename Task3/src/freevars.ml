open Grammar

module H = Hashtbl
   
let freevars expr =
    let delVars = (H.create 32 : (string, bool) H.t) in
    let ans = (H.create 32 : (string, bool) H.t) in 
    let rec fv = function
    | Var s       -> if (not (H.mem delVars s)) then H.replace ans s true
    | Abst (x, e) -> H.add delVars x true;
                     fv e;
                     H.remove delVars x
    | Appl (a, b) -> fv a;
                     fv b
    in
    fv expr;
    ans;;