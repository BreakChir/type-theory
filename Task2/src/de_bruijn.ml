open Grammar

module H = Hashtbl
   
let to_de_bruijn expr =
    let fv = (H.create 256 : (string, int) H.t) in
    let int_to_var = (H.create 256 : (int, string) H.t) in
    let var_lvl = (H.create 100_000 : (string, int) H.t) in
    let fv_counter = ref 0 in
    
    let rec build lvl = function
        | Var s       -> if (H.mem var_lvl s) then
                            DVar (lvl - (H.find var_lvl s))
                         else 
                            if (H.mem fv s) then
                                DVar (H.find fv s)
                            else begin
                                fv_counter := !fv_counter - 1;
                                H.add int_to_var !fv_counter s;
                                H.add fv s !fv_counter;
                                DVar !fv_counter
                            end
        | Abst (x, e) -> H.add var_lvl x (lvl + 1);
                         let ret = DAbst (build (lvl + 1) e) in
                         H.remove var_lvl x;
                         ret
        | Appl (l, r) -> DAppl (build lvl l, build lvl r)
    in
    (build 0 expr, int_to_var);;