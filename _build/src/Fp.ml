module type LabelType =
sig
  type t
  val convert_to_string : t->string
  val convert_to_t : string -> t
end

module type FormulePropositionnelle =
sig
  type t 
  type variable = { name : t; value : bool; }
  val var_false : variable->variable
  val var_true : variable -> variable
  val get_value : variable -> bool
  val get_name : variable -> t
  type fp =
    |V of variable 
    |T
    |F
    |Not of fp
    |Comp of fp * connector * fp
  and connector = C_and |C_or |C_imp |C_equi
  val is_fp_true : fp -> bool
  val is_fp_false : fp -> bool
  type tree = TT | TF | TN of tree * tree
  val bool_of_tree : tree->bool
  val get_var_position : fp -> variable -> tree
  val partial_eval : fp -> variable -> tree -> fp
  val fval : fp -> fp
end

module Make_Fp(Elt: LabelType):(FormulePropositionnelle with type t=Elt.t)=
struct
  type t= Elt.t

  type variable = {name : t; value : bool}

  let var_false v = {v with value=false}

  let var_true v = {v with value=true}

  let get_value v = v.value

  let get_name v= v.name
  type fp =
    |V of variable 
    |T
    |F
    |Not of fp
    |Comp of fp * connector * fp
  and connector = C_and |C_or |C_imp |C_equi

  let fp_not f_p = if f_p = T then F else if f_p = F then T else Not f_p

  type tree = TT | TF | TN of tree * tree

  let get_var_position (f_p:fp) (v_to_eval:variable) : tree =
    let v_name = v_to_eval.name in
    let rec aux = function
      |T|F -> TF
      |Not fp1 -> aux fp1
      |V v -> if v.name = v_name then TT else TF
      |Comp (fp1,_,fp2) -> 
        let r1 = aux fp1 in 
        let r2 = aux fp2 in
        if r1=TF && r2=TF then TF else TN (r1,r2)
    in aux f_p

  let bool_of_tree tr = tr<>TF

  let is_fp_true f_p = f_p = T

  let is_fp_false f_p = f_p = F

  let rec connect (c:connector) (a:fp) (b:fp) =
    match c with
    |C_and -> if a=T then b else F
    |C_or ->  
      begin
        match (a,b) with
        |(F,b) -> b
        |(a,F) -> a
        |(T,_) -> T
        |(_,T) -> T
        |_ -> Comp (a,C_or ,b)
      end
    |C_imp -> connect C_or (fp_not a) b 
    |C_equi -> connect C_and (connect C_imp a b) (connect C_imp b a) 

  let partial_eval (f_p:fp) (v_to_eval:variable) (var_pos:tree) : fp =
    let rec aux = function
      |(TT,V _) -> if v_to_eval.value then T else F
      |(TT,(Not fp1)) -> 
        begin 
          let r1 = aux (TT,fp1) in 
          match r1 with T|F -> fp_not r1 | r1 -> Not r1 
        end
      |(TN (t1,t2) , Comp (fp1,c,fp2)) -> connect c (aux (t1,fp1)) (aux (t2,fp2))
      |(_,f_p) -> f_p
    in aux (var_pos,f_p)

  let rec fval f_p = 
    match f_p with
    | V v -> if v.value then T else F
    | Not fp1 -> fp_not fp1
    | Comp (fp1,c,fp2) -> connect c (fval fp1) (fval fp2)
    | fp1 -> fp1
end