open Grammar
    
let substit expr1 expr2 =
    let rec change_lvl add lvl = function
        | DVar n       -> if (n >= lvl) then DVar (n + add) else DVar n
        | DAbst e      -> DAbst (change_lvl add (lvl + 1) e)
        | DAppl (a, b) -> DAppl (change_lvl add lvl a, change_lvl add lvl b)
    in

    let rec subst lvl = function
        | DVar n       -> if (lvl = n) then
                            change_lvl lvl 0 expr2
                          else 
                            DVar (if (n > lvl) then (n - 1) else n)
        | DAbst e      -> DAbst (subst (lvl + 1) e)
        | DAppl (a, b) -> DAppl (subst lvl a, subst lvl b)
    in
    subst 0 expr1;;
    
let give_normal_form expr = 
    let rec one_reduction expr = match expr with
        | DAppl (DAbst e1, e2)        -> (substit e1 e2, true)
        | DAppl (a, b)                -> let (inner_a, is_reduct_a) = one_reduction a in
                                         if (is_reduct_a) then
                                             (DAppl (inner_a, b), true)
                                         else
                                             let (inner_b, is_reduct_b) = one_reduction b in
                                             (DAppl (a, inner_b), is_reduct_b)
        | DAbst e                    -> let (inner, is_reduct) = one_reduction e in
                                            (DAbst inner, is_reduct)
        | DVar s                     -> (expr, false)
    in
    let rec reduct expr =
        let (new_expr, is_reduct) = one_reduction expr in
        if (is_reduct) then
            reduct new_expr
        else
            new_expr
    in
    reduct expr;;