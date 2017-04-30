type t= string

type variable = {name : t; value : bool}

type var_set = (t,variable) Hashtbl.t

type fp =
  |V of variable 
  |T
  |F
  |Not of fp
  |Comp of fp * connector * fp
and connector = C_and |C_or |C_imp |C_equi

let rec fval f_p =
  match f_p with
  | V v -> v.value
  | T -> true
  | F -> false
  | Not fp1 -> not (fval fp1)
  | Comp (fp1,c,fp2) -> connect c (fval fp1) (fval fp2)
and connect (c:connector) (a:bool) (b:bool) =
  match c with
  |C_and -> a && b
  |C_or -> a || b
  |C_imp -> (not a) || b
  |C_equi -> connect C_and (connect C_imp a b) (connect C_imp b a) 

let p = {name="p";value=true};;
let q = {name="q";value=false};;
let r = {name="r";value=true};;
let s = {name="s";value=true};;

let fp1 = Comp ((Comp(V p, C_imp, V q)),C_or,(Comp (V r,C_and, V s)));;

let()= if fval fp1 then print_endline "True" else print_endline "False" ;;