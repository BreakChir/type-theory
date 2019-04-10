open Grammar
open Printf

module H = Hashtbl

let is_not_solv = ref false;;
let var_not_subst = (H.create 256 : (string, _) H.t);;

let solve' e1 e2 expr_list = 
    match e1 with
        | Impl (a, b) -> (match e2 with
            | VarI x      -> (true, (e2 @= e1) :: expr_list)
            | Impl (c, d) -> (true, (a @= c) :: (b @= d) :: expr_list))
        | VarI x      -> (false, (e1 @= e2) :: expr_list);;

let solve expr_list = function
    | Eq (a, b) -> if (a = b) then
                      (false, expr_list)
                   else
                      solve' a b expr_list;;

let rec is_contains_var x = function
    | VarI a      -> x = a
    | Impl (a, b) -> (is_contains_var x a) || (is_contains_var x b);;

let try_subst' e1 e2 m =
    match e1 with
        | Impl (a, b) -> (false, m)
        | VarI x      -> if (is_contains_var x e2) then begin
                            is_not_solv := true;
                            (false, [])
                         end   
                         else if (H.mem var_not_subst x) then (false, m)
                         else begin
                            H.add var_not_subst x true;
                            let is_chng = ref false in
                            let rec substitute x t = function
                                | Impl (a, b) -> (substitute x t a) --> (substitute x t b)
                                | VarI s      -> if (s = x) then begin 
                                                     is_chng := true;
                                                     t
                                                 end    
                                                 else VarI s
                            in
                            
                            let subst = function
                                | Eq (a, b) -> if (a = e1 && b = e2) then a @= b
                                               else (substitute x e2 a) @= (substitute x e2 b)
                            in
                            
                            let m1 = List.map subst m in
                            (!is_chng, m1)
                         end;;

let try_subst m = function
    | Eq (a, b) -> try_subst' a b m;;

let rec unification expr_list =
    let rec unif' = function
        | []      -> (false, [])
        | x :: xs -> let (isChng, l) = unif' xs in
                     let (isChng', m) = solve l x in
                     (isChng || isChng', m)
    in
    
    let rec unif'' m = function
        | []      -> (false, m)
        | x :: xs -> let (isChng, m1) = try_subst m x in
                     if (isChng) then (true, m1) else
                        unif'' m1 xs
    in
    
    let (isChng, m1) = unif' expr_list in
    let (isChng', m2) = unif'' m1 m1 in
    if (!is_not_solv) then
        None
    else
        if (isChng || isChng') then
            unification m2
        else
            Some m2;;