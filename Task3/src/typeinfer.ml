open Grammar
open Buffer
open Unification
open Freevars
open Printf

module H = Hashtbl

let type_infer expr =
    let free_vars = freevars expr in
    let var_type = (H.create 256 : (string, string) H.t) in
    let types = (H.create 64 : (string, string) H.t) in
    let numVar = ref 0 in
    let unif = ref [] in
    
    let get_new_type () =
        numVar := !numVar + 1;
        "t" ^ (string_of_int !numVar)
    in

    let rec get_unif expr = match expr with
        | Var x       -> if (H.mem var_type x) then
                            VarI (H.find var_type x)
                         else begin
                            let new_type = get_new_type () in
                            H.add var_type x new_type;
                            VarI new_type
                         end
        | Abst (x, e) -> let new_type = get_new_type () in
                         H.add var_type x new_type;
                         let type' = get_unif e in
                         (VarI new_type) --> type'
        | Appl (a, b) -> let type_a = get_unif a in
                         let type_b = get_unif b in
                         let new_type = VarI (get_new_type ()) in
                         unif := (type_a @= (type_b --> new_type)) :: !unif;
                         new_type
    in
    let _ = get_unif expr in

    let put_type = function
        | Eq (a, b) -> H.add types (string_of_type a) (string_of_type b)
    in            
    
    let is_no_type = match unification !unif with
        | Some m -> List.iter put_type m;
                    false
        | None   -> true
    in

    let get_vartype var =
        let type' = H.find var_type var in
        if (H.mem types type') then
            H.find types type'
        else
            type'
    in

    let string_of_fvars =
        let buf = create 1000 in
        let put_to_context var a =
            if ((length buf) > 0) then
                add_string buf ", ";
            add_string buf var;
            add_string buf " : ";
            add_string buf (get_vartype var)
        in
        H.iter put_to_context free_vars;
        contents buf
    in

    let string_of_prove expr =
        let rec add_space lvl =
            if (lvl > 0) then "*   " ^ (add_space (lvl - 1)) else ""
        in
        
        let get_var_str var =
            var ^ " : " ^ (get_vartype var)
        in

        let rec add_vars was_var = function
            | []      -> ""
            | x :: xs -> (if (was_var || String.length string_of_fvars > 0) then ", " else "") ^
                            (get_var_str x) ^ (add_vars true xs)
        in

        let rec get_exprtype = function
            | VarI t      -> if (H.mem types t) then (H.find types t) else t
            | Impl (a, b) -> "(" ^ (get_exprtype a) ^ " -> " ^ (get_exprtype b) ^ ")"
        in
        
        let gel_line lvl fix_vars type' rule expr =
            let buf = create 1000 in
            let assump = string_of_fvars ^ (add_vars false fix_vars) in
            add_string buf (add_space lvl);
            add_string buf assump;
            if String.length assump > 0 then
                add_char buf ' ';
            add_string buf "|- ";
            add_string buf (string_of_expr expr);
            add_string buf " : ";
            add_string buf (get_exprtype type');
            add_string buf rule;
            contents buf

        in

        let rec string_of_prove' lvl fix_vars expr =
            match expr with
            | Var x       -> let type' = if (H.mem var_type x) then
                                            H.find var_type x
                                         else begin
                                            let new_type = get_new_type () in
                                            H.replace var_type x new_type;
                                            new_type
                                         end
                            in
                            (VarI type', [gel_line lvl fix_vars (VarI type') " [rule #1]\n" expr])
            | Abst (x, e) -> let new_type = get_new_type () in
                             H.replace var_type x new_type;
                             let (type', m) = string_of_prove' (lvl + 1) (x :: fix_vars) e in
                             let ntype' = (VarI new_type) --> type' in
                             let line = gel_line lvl fix_vars ntype' " [rule #3]\n" expr in 
                             (ntype', line :: m)
            | Appl (a, b) -> let (type_a, ma) = string_of_prove' (lvl + 1) fix_vars a in
                             let (type_b, mb) = string_of_prove' (lvl + 1) fix_vars b in
                             let type' = VarI (get_new_type ()) in
                             let line = gel_line lvl fix_vars type' " [rule #2]\n" expr in
                             (type', line :: (ma @ mb))
        in
        let (_, m) = string_of_prove' 0 [] expr in
        m
    in
    numVar := 0;
    H.reset var_type;
    if (is_no_type) then ["Expression has no type\n"]
    else string_of_prove expr;;