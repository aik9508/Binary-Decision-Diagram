module CS = Core.Std 
open Lt

module type FormulePropositionnelle =
sig
  type t 
  type variable
  val var_false : variable -> variable
  val var_true : variable  -> variable
  val get_value : variable -> bool
  val get_name : variable  -> t
  val create_var_true : t -> variable
  val create_var_false : t -> variable
  type fp
  val fp_true : unit -> fp 
  val fp_false : unit -> fp
  val fp_variable : variable-> fp
  val fp_and : fp -> fp -> fp
  val fp_or : fp -> fp -> fp
  val fp_imp : fp -> fp -> fp
  val fp_equi : fp -> fp -> fp
  val fp_not : fp -> fp
  val is_fp_true : fp -> bool
  val is_fp_false : fp -> bool
  val string_of_fp : fp -> string
  type tree 
  val bool_of_tree : tree->bool
  val get_var_position : fp -> variable-> tree
  val get_all_variables : fp -> variable list
  val partial_eval : fp -> variable -> tree -> fp
  val partial_eval_i : fp -> t list -> int -> bool -> fp
  val partial_eval_l : fp -> t list -> int list -> bool list -> fp
  val is_satisfiable_s : string -> bool
  val is_satisfiable_fp : ? var_list : variable list -> fp -> bool
  val is_valid_s : string -> bool
  val is_valid_fp : ? var_list : variable list -> fp -> bool
  val factorise : ?var_list:variable list -> fp -> int list * int list * fp
  val fval : fp -> fp
  val fp_of_string : string -> fp * variable list
  val eval : fp -> ?bl:bool list -> t list -> bool
end

module Make_Fp(Elt: LabelType) :(FormulePropositionnelle with type t=Elt.t) =
struct
  type t= Elt.t

  type variable = {name : t; value : bool}

  let var_false v = {v with value=false}

  let var_true v = {v with value=true}

  let get_value v = v.value

  let get_name v= v.name

  let create_var_true name = {name=name ; value=true}

  let create_var_false name = {name=name ; value=false}

  type fp =
    |V of variable
    |T
    |F
    |Not of fp
    |Comp of fp * connector * fp
  and connector = C_and |C_or |C_imp |C_equi

  let fp_not f_p = 
    match f_p with
    | T -> F 
    | F -> T
    | Not fp1 -> fp1
    | _ -> Not f_p

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

  let fp_true () = T

  let fp_false () = F

  let fp_variable v = V v

  let rec connect (c:connector) (a:fp) (b:fp) =
    match c with
    |C_and -> 
      begin
        match (a,b) with
        |(T,b)->b
        |(a,T)->a
        |(F,_)->F
        |(_,F)->F
        |(V a',Not(V b'))-> if a'.name = b'.name then F else Comp (a,C_and,b)
        |(Not (V a'), V b') -> if a'.name = b'.name then F else Comp (a,C_and,b)
        |(V a',V b')-> if a'.name = b'.name then a else Comp(a, C_and,b)
        |(Not (V a'), Not (V b')) -> if a'.name = b'.name  then a else Comp(a,C_and,b)
        | _ -> Comp(a, C_and, b)
      end
    |C_or ->  
      begin
        match (a,b) with
        |(F,b) -> b
        |(a,F) -> a
        |(T,_) -> T
        |(_,T) -> T
        |(V a',Not(V b'))-> if a'.name = b'.name then T else Comp (a,C_or,b)
        |(Not (V a'), V b') -> if a'.name = b'.name then T else Comp (a,C_or,b)
        |(V a',V b')-> if a'.name = b'.name then a else Comp(a, C_or,b)
        |(Not (V a'), Not (V b')) -> if a'.name = b'.name  then a else Comp(a,C_or,b)
        |_ -> Comp (a,C_or ,b)
      end
    |C_imp -> connect C_or (fp_not a) b 
    |C_equi -> connect C_and (connect C_imp a b) (connect C_imp b a )

  let fp_and fp1 fp2 = connect C_and fp1 fp2

  let fp_or fp1 fp2 = connect C_or fp1 fp2

  let fp_imp fp1 fp2 = connect C_imp fp1 fp2

  let fp_equi fp1 fp2 = connect C_equi fp1 fp2

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

  let partial_eval (f_p:fp) (v_to_eval:variable) (var_pos:tree) : fp =
    let rec aux = function
      |(TT,V _) -> if v_to_eval.value then T else F
      |(TT,(Not fp1)) -> fp_not (aux (TT,fp1))
      |(t, Not fp1) -> fp_not (aux (t,fp1))
      |(TN (t1,t2) , Comp (fp1,c,fp2)) -> connect c (aux (t1,fp1)) (aux (t2,fp2))
      |(_,f_p) -> f_p
    in aux (var_pos,f_p)

  let partial_eval_i (f_p:fp) (v_name_list:t list) (i:int) (b:bool)=
    let v_name = List.nth v_name_list i in
    let v_to_eval = {name=v_name; value = b} in
    let var_pos=get_var_position f_p v_to_eval in
    partial_eval f_p v_to_eval var_pos 

  let partial_eval_l (f_p:fp) (v_name_list:t list) (l:int list) (bl:bool list) =
    List.fold_left2 (fun f_p i b -> partial_eval_i f_p v_name_list i b) f_p  l bl 

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

  let remove_not s =
    if String.get s 0 ='~' then String.sub s 1 (String.length s - 1)  else s

  let var_filter s = String.get s 0 <> '[' && s <> "true" && s<> "false"

  let get_all_v_names s =
    let blank_reg = Str.regexp "[ \t]+" in
    let s_without_blank = Str.global_replace blank_reg "" s in
    let separator_reg = 
      Str.regexp ("[" ^
                  (c_reg_list
                   |> List.map (fun s -> "\\("^s^"\\)") 
                   |> String.concat "|") ^ "]+") 
    in
    s_without_blank
    |> Str.split separator_reg
    |> List.map remove_not
    |> CS.List.dedup ~compare:String.compare
    |> List.filter var_filter  

  let add_variable var_set v_name= 
    match CS.Hashtbl.find var_set v_name with
    |None ->
      CS.Hashtbl.add_exn var_set ~key:v_name ~data:({name=(Elt.convert_to_t v_name); value=false});
    |_->()

  let update_variable s var_set = List.iter (add_variable var_set) (get_all_v_names s)

  let get_all_variables f_p =
    let var_set = CS.String.Table.create ~size:10 () in
    let rec aux = function
      |T|F -> ()
      |Not fp1 -> aux fp1
      |Comp (fp1,_,fp2) -> aux fp1; aux fp2
      |V v -> let v_name = Elt.convert_to_string v.name in
        if not (CS.Hashtbl.mem var_set v_name) then 
          CS.Hashtbl.add_exn var_set ~key:v_name ~data:v in
    aux f_p ;
    CS.Hashtbl.data var_set

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
      let l = logic_separate s in
      if List.length l = 1 then single_fp_of_string s else
        Comp (aux (List.nth l 1), connector_of_string (List.nth l 0) ,aux (List.nth l 2))
    and single_fp_of_string s =
      match s with
      |"true" -> T
      |"false" -> F
      |s -> 
        if String.get s 0 = '~' then 
          fp_not (single_fp_of_string (remove_not s))
        else if String.get s 0 = '[' then
          CS.Hashtbl.find_exn higher_order_var_set s
        else
          V (CS.Hashtbl.find_exn var_set s)
    in aux s

  let fp_of_string s = 
    let var_set = CS.String.Table.create() in
    let higher_order_var_set = CS.String.Table.create() in
    let result = ref T in
    rm_parenthesis s |>
    List.iteri (fun i s -> 
        update_variable s var_set;
        result:=fp_of_string_ s var_set higher_order_var_set;
        CS.Hashtbl.add_exn higher_order_var_set ~key:("["^string_of_int i^"]") ~data:!result);
    (!result , (CS.Hashtbl.data var_set))

  let is_satisfiable_ (f_p:fp) (var_list:variable list)=
    let rec aux f_p var_list =
      match var_list with 
      |[] -> f_p = T
      |v :: tl ->
        let var_pos = get_var_position f_p v in
        let fp1= partial_eval f_p {v with value = false} var_pos in
        if fp1 = T then true
        else begin
          let fp2 = partial_eval f_p {v with value = true} var_pos in
          if fp2 = T then true else aux fp1 tl || aux fp2 tl
        end
    in aux f_p var_list

  let is_satisfiable_fp ?(var_list=[]) f_p  =
    if var_list = [] then is_satisfiable_ f_p (get_all_variables f_p)
    else is_satisfiable_ f_p var_list

  let is_satisfiable_s s =
    let (f_p, var_list) = fp_of_string s in
    is_satisfiable_ f_p var_list

  let is_valid_ (f_p:fp) (var_list:variable list)=
    let rec aux f_p var_list =
      match var_list with 
      |[] -> f_p = T
      |v :: tl ->
        let var_pos = get_var_position f_p v in
        let fp1= partial_eval f_p {v with value = false} var_pos in
        if fp1 = F then false
        else begin
          let fp2 = partial_eval f_p {v with value = true} var_pos in
          if fp2 = F then false else aux fp1 tl && aux fp2 tl
        end
    in aux f_p var_list

  let is_valid_fp ?(var_list=[]) f_p  =
    if var_list = [] then is_valid_ f_p (get_all_variables f_p)
    else is_valid_ f_p var_list

  let is_valid_s s =
    let (f_p, var_list) = fp_of_string s in
    is_valid_ f_p var_list

  let eval (f_p:fp) ?(bl=[]) (v_name_list: t list)  =
    let n = List.length v_name_list in
    if bl = [] then (fval f_p) = T 
    else begin
      assert (n = List.length bl);
      let l = List.mapi (fun i _ -> i) (Array.to_list (Array.make n 0)) in
      (partial_eval_l f_p v_name_list l bl) =T
    end

  let factorise_ f_p var_list =
    let rec aux i true_list false_list f_p var_list =
      match var_list with
      |[] -> true_list,false_list,f_p 
      |v :: tl -> let t = get_var_position f_p v in
        begin
          let ff = partial_eval f_p {v with value=false} t in
          let ft = partial_eval f_p {v with value=true} t in
          if ff = F then
            aux (i+1) (i::true_list) false_list ft tl 
          else  
            begin
              if ft = F then
                aux (i+1) true_list (i::false_list) ff tl
              else
                aux (i+1) true_list false_list f_p tl
            end
        end
    in aux 0 [] [] f_p var_list

  let factorise ?(var_list=[]) (f_p:fp)  = 
    if var_list = [] then factorise_ f_p (get_all_variables f_p) 
    else factorise_ f_p var_list

end