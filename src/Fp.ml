open Core.Std

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
  val string_of_fp : fp -> string
  type tree = TT | TF | TN of tree * tree
  val bool_of_tree : tree->bool
  val get_var_position : fp -> variable -> tree
  val partial_eval : fp -> variable -> tree -> fp
  val fval : fp -> fp
  val remove_not : string -> string
  val var_filter :  string->bool
  val get_all_v_names : string -> string list
  val logic_separate : string -> string list
  val connector_of_string : string->connector
  val fp_of_string : string -> fp * variable list
  val rm_parenthesis : string -> string list
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

  let string_of_connector = function
    |C_equi -> "<=>"
    |C_imp -> "=>"
    |C_or ->"||"
    |C_and -> "&&"

  let rec string_of_fp f_p =
    match f_p with
    |T -> "T"
    |F -> "F"
    |V v -> Elt.convert_to_string v.name
    |Not f_p -> "(~"^string_of_fp f_p^")" 
    |Comp (fp1,c,fp2) -> "("^string_of_fp fp1^string_of_connector c^string_of_fp fp2^")"

  let rec connect (c:connector) (a:fp) (b:fp) =
    match c with
    |C_and -> 
      begin
        match (a,b) with
        |(T,b)->b
        |(a,T)->a
        |(F,_)->F
        |(_,F)->F
        |(V a',Not(V b'))-> if a'.name=b'.name then F else Comp (a,C_and,b)
        |(Not (V a'), V b') -> if a'.name =b'.name then F else Comp (a,C_and,b)
        |(V a',V b')-> if a'.name = b'.name then a else Comp(a, C_and,b)
        |(Not (V a'), Not (V b')) -> if a'.name =b'.name  then a else Comp(a,C_and,b)
        | _ -> Comp(a, C_and, b)
      end
    |C_or ->  
      begin
        match (a,b) with
        |(F,b) -> b
        |(a,F) -> a
        |(T,_) -> T
        |(_,T) -> T
        |(V a',Not(V b'))-> if a'.name=b'.name then T else Comp (a,C_or,b)
        |(Not (V a'), V b') -> if a'.name =b'.name then T else Comp (a,C_or,b)
        |(V a',V b')-> if a'.name = b'.name then a else Comp(a, C_or,b)
        |(Not (V a'), Not (V b')) -> if a'.name =b'.name  then a else Comp(a,C_or,b)
        |_ -> Comp (a,C_or ,b)
      end
    |C_imp -> connect C_or (fp_not a) b 
    |C_equi -> connect C_and (connect C_imp a b) (connect C_imp b a )

  let partial_eval (f_p:fp) (v_to_eval:variable) (var_pos:tree) : fp =
    let rec aux = function
      |(TT,V _) -> if v_to_eval.value then T else F
      |(TT,(Not fp1)) -> fp_not (aux (TT,fp1))
      |(t, Not fp1) -> fp_not (aux (t,fp1))
      |(TN (t1,t2) , Comp (fp1,c,fp2)) -> connect c (aux (t1,fp1)) (aux (t2,fp2))
      |(_,f_p) -> f_p
    in aux (var_pos,f_p)

  let rec fval f_p = 
    match f_p with
    | V v -> if v.value then T else F
    | Not fp1 -> fp_not fp1
    | Comp (fp1,c,fp2) -> connect c (fval fp1) (fval fp2)
    | fp1 -> fp1

  let rm_parenthesis str_fp =
    let r = Str.regexp "(\\([][A-Za-z0-9<=>\\| &~]+\\))" in
    let temporal_variable_name = ref 0 in
    let separate_fp = ref [] in
    let rec reduce str_fp =
      let current_str_fp = ref str_fp in
      let rec aux str_fp =
        begin
          try 
            let a = Str.search_forward r str_fp 0 in
            assert (a>=0);
            let substr_without_parenthesis= Str.matched_group 1 str_fp in
            let substr_with_parenthesis = Str.matched_string str_fp in
            let r_substr = Str.regexp_string substr_with_parenthesis in
            current_str_fp := Str.global_replace r_substr ("["^string_of_int !temporal_variable_name^"]") !current_str_fp;
            (*print_string !current_str_fp;*)
            temporal_variable_name:=!temporal_variable_name +1 ;
            reduce substr_without_parenthesis ;
            aux !current_str_fp
          with _ -> 
            separate_fp := !separate_fp @ [!current_str_fp]
        end
      in aux str_fp 
    in reduce str_fp;
    !separate_fp

  let c_reg_list = ["<=>";"=>";"\\|\\|";"&&"]
  let c_reg_list_2 = ["<=>";"=>";"[\\|]+";"&&"]

  (*let c_list = [C_equi;C_imp;C_or;C_and]*)

  let remove_not s =
    if String.get s 0 ='~' then String.sub s ~pos:1 ~len:(String.length s - 1)  else s

  let var_filter s = String.get s 0 <> '[' && s <> "true" && s<> "false"

  let get_all_v_names s =
    let blank_reg = Str.regexp "[ \t]+" in
    let s_without_blank = Str.global_replace blank_reg "" s in
    let separator_reg = 
      Str.regexp ("[" ^
                  (c_reg_list
                   |> List.map ~f:(fun s -> "\\("^s^"\\)") 
                   |> String.concat ~sep:"|") ^ "]+") 
    in
    let v_list' = 
      s_without_blank
      |> Str.split separator_reg
      |> List.map ~f:remove_not
      |> List.dedup ~compare:String.compare in
    List.filter ~f:var_filter v_list' 

  let add_variable var_set v_name= 
    match Hashtbl.find var_set v_name with
    |None ->
      print_string "before : var_set : ";
      print_int (Hashtbl.length var_set);
      print_string "\n";
      Hashtbl.add_exn var_set ~key:v_name ~data:({name=(Elt.convert_to_t v_name); value=false});
      print_string "after : var_set : ";
      print_int (Hashtbl.length var_set);
      print_string "\n"
    |_->()

  let update_variable s var_set = List.iter ~f:(add_variable var_set) (get_all_v_names s)

  let logic_separate s =
    if (s="true" || s="false") then [s] 
    else
      let rec aux = function
        |[] -> [s]
        |hd::tl -> 
          let l= Str.bounded_split (Str.regexp hd) s 2 in
          if List.length l = 1 then aux tl else hd :: l 
      in aux c_reg_list_2

  exception Invalid_argument

  let connector_of_string = function
    |"<=>" -> C_equi
    |"=>" -> C_imp
    |"[\\|]+" -> C_or
    |"&&" -> C_and
    |_ -> raise Invalid_argument

  let fp_of_string_ s var_set higher_order_var_set=
    let rec aux s =
      print_string ("fragment: "^s^"\n");
      let l = logic_separate s in
      if List.length l = 1 then single_fp_of_string s else
        Comp (aux (List.nth_exn l 1), connector_of_string (List.nth_exn l 0) ,aux (List.nth_exn l 2))
    and single_fp_of_string s =
      match s with
      |"true" -> T
      |"false" -> F
      |s -> 
        if String.get s 0 = '~' then 
          fp_not (single_fp_of_string (remove_not s))
        else if String.get s 0 = '[' then
          (print_string ("high_order"^s^"\n");
           Hashtbl.find_exn higher_order_var_set s)
        else
          (print_string ("var_set"^s^"\n") ;
           V (Hashtbl.find_exn var_set s))
    in aux s

  let fp_of_string s = 
    let var_set = String.Table.create() in
    let higher_order_var_set = String.Table.create() in
    let result = ref T in
    rm_parenthesis s |>
    List.iteri ~f:(fun i s -> 
        update_variable s var_set;
        result:=fp_of_string_ s var_set higher_order_var_set;
        print_string "before : higher_order: ";
        print_int (Hashtbl.length higher_order_var_set);
        print_string "\n";
        Hashtbl.add_exn higher_order_var_set ~key:("["^string_of_int i^"]") ~data:!result;
        print_string "after : higher_order: ";
        print_int (Hashtbl.length higher_order_var_set);
        print_string "\n");
    (!result , (Hashtbl.data var_set))
end