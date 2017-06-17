open Fp
open Lt

module StrLabel:(LabelType with type t = string)=
struct
  type t = string
  let convert_to_string s = s
  let convert_to_t s = s
  let hash x = Hashtbl.hash x
  let equal x y = x = y
end

module type Bdd = sig
  type t
  module Fp : FormulePropositionnelle with type t:=t
  type fp = Fp.fp
  type variable = Fp.variable
  type bdd
  exception BDDException of string
  val bdd_of_fp : ?var_list: variable list -> fp -> bdd
  val bdd_of_fp_l : t list -> fp -> bdd
  val factorise : ?var_list:variable list -> fp -> variable list * variable list * fp * variable list
  val print_factorized_fp : variable list * variable list * fp * variable list -> unit
  val number_of_solution : bdd -> int
  val bdd_of_file : string -> bdd
  val string_of_bdd : bdd -> string
  val dump_s : string -> unit
  val dump_fp : ?var_list : variable list -> fp -> unit
  val one_solution : bdd -> bool * t list
  val is_satisfiable : bdd -> bool
  val is_valid : bdd -> bool
end

module Make_bdd(Elt:LabelType) : Bdd with type t:=Elt.t =
struct
  module Fp = Make_Fp(Elt)
  type t = Elt.t
  type fp = Fp.fp
  type variable = Fp.variable
  exception BDDException of string

  module Index = struct
    type t = I of int | T | F
    let hash x = match x with I i -> Hashtbl.hash i | T -> -1 | F -> -3
    let equal x y = match x,y with (T,T)|(F,F) -> true | (I i1, I i2)-> i1=i2 | _ ->false 
    let to_string x = match x with I i -> string_of_int i| T -> "@t"| F -> "@f"
    let to_index s = match s with "@t" -> T | "@f"-> F | s -> I (int_of_string s)
    let to_int x = match x with I i -> i | T -> -1 | F -> -3
  end
  type index = Index.t

  module Standard_data = 
  struct
    type t =  {
      label: Elt.t;
      nt: index;
      nf: index;
    }
    let hash x = 13 * (Elt.hash x.label) + 31*(Index.hash x.nt) + 97 * (Index.hash x.nf)
    let equal x y = (Elt.equal x.label y.label) && Index.equal x.nt y.nt && Index.equal x.nf y.nf
    let next_true x= x.nt
    let next_false x = x.nf
    let label x = x.label
    let to_string x = String.concat " " [Elt.convert_to_string x.label; Index.to_string x.nt ; Index.to_string x.nf]
    let std_data label nt nf = {label;nt;nf}
  end
  type data_piece = Standard_data.t
  type bdd = Trivial of t list * bool | NonTrivial of t list * ((int, data_piece) Hashtbl.t)

  let bdd_of_file (file:string) =
    let h = Hashtbl.create 10 in
    let label_list = ref [] in
    begin
      let ic = open_in file in
      try
        while true do
          let line = input_line ic in 
          let line_data_str = String.split_on_char ' ' line in
          let line_data =
            match line_data_str with
            |[key;label;next_true;next_false]-> 
              let lab = Elt.convert_to_t label in
              if not (List.exists ( Elt.equal lab ) !label_list) then
                label_list := !label_list @ [lab] ;
              (int_of_string key, Standard_data.std_data lab
                 (Index.to_index next_true) 
                 (Index.to_index next_false))
            | _ -> raise (BDDException "Incorrect data format") in
          Hashtbl.add h (fst line_data) (snd line_data)
        done
      with End_of_file ->
        close_in_noerr ic
    end;
    NonTrivial (!label_list, h)

  exception Unmatched_arguments

  let change_data_piece data_p max_index= 
    let change_index ind = if ind=Index.T || ind=Index.F  then ind else Index.I (max_index - Index.to_int ind) in
    Standard_data.std_data 
      (Standard_data.label data_p) 
      (change_index (Standard_data.next_true data_p))
      (change_index (Standard_data.next_false data_p))

  let bdd_of_fp_ (f_p:fp) (var_list:variable list) =
    let module Sd = Standard_data in
    let module Ht = Hashtbl.Make(Standard_data) in
    let h = Ht.create 20 in
    let current_index = ref 0 in
    let data_list = ref [] in
    let rec aux f_p var_list = 
      match var_list with
      |[] -> 
        if Fp.is_fp_true f_p then Index.T
        else if Fp.is_fp_false f_p then Index.F 
        else raise Unmatched_arguments
      | s :: l ->
        begin
          let var_pos = Fp.get_var_position f_p s in
          if Fp.bool_of_tree var_pos then
            let leftFp = Fp.partial_eval f_p (Fp.var_false s) var_pos in
            let false_index = aux leftFp l in
            let rightFp = Fp.partial_eval f_p (Fp.var_true s) var_pos in
            let true_index = aux rightFp l in
            let data_p = Sd.std_data (Fp.get_name s) true_index false_index in
            if not (Ht.mem h data_p) then
              begin
                if Index.equal true_index false_index then true_index
                else
                  begin
                    Ht.add h data_p !current_index;
                    data_list := data_p :: !data_list;
                    current_index:=!current_index +1;
                    Index.I (!current_index -1)
                  end
              end
            else
              Index.I (Ht.find h data_p) 
          else
            aux f_p l
        end
    in match aux f_p var_list with
    | Index.T -> Trivial (List.map Fp.get_name var_list, true)
    | Index.F -> Trivial (List.map Fp.get_name var_list, false)
    | _ -> 
      let max_index=List.length !data_list - 1 in
      let htl = Hashtbl.create (List.length !data_list) in
      List.iteri (fun i data_p -> Hashtbl.add htl i (change_data_piece data_p max_index)) !data_list;
      NonTrivial (List.map Fp.get_name var_list,htl)

  let bdd_of_fp ?(var_list=[]) f_p =
    if var_list = [] then bdd_of_fp_ f_p (Fp.get_all_variables f_p)
    else bdd_of_fp_ f_p var_list

  let bdd_of_fp_l v_list f_p =
    bdd_of_fp_ f_p (List.map Fp.create_var_false v_list)

  let string_of_bdd = function
    | Trivial (_, true) -> "true"
    | Trivial (_, false) -> "false"
    | NonTrivial (_, h) ->
      let rec aux ind l =
        if Hashtbl.mem h ind then 
          aux (ind+1) (l @ [Hashtbl.find h ind])
        else
          String.concat "\n" (
            List.mapi (fun i data_p -> (string_of_int i) ^ " " ^ Standard_data.to_string data_p) l
          )
      in aux 0 []

  let factorise_ f_p var_list=
    let module Sd = Standard_data in
    match bdd_of_fp_ f_p var_list with
    | Trivial (_, true) -> ([],[],Fp.fp_true(),[])
    | Trivial (_, false) -> ([],[],Fp.fp_false(),[])
    | NonTrivial (_,h) ->
      let rec aux data_p = 
        let v = Fp.fp_variable (Fp.create_var_false (Sd.label data_p)) in
        let fp_right = get_data_p (Sd.next_true data_p) in
        let fp_left = get_data_p (Sd.next_false data_p) in 
        Fp.fp_or (Fp.fp_and (Fp.fp_not v) fp_left) (Fp.fp_and v fp_right)
      and get_data_p = function
        |Index.T -> Fp.fp_true()
        |Index.F -> Fp.fp_false()
        |Index.I i -> aux (Hashtbl.find h i) in
      match Fp.factorise ~var_list:var_list (aux (Hashtbl.find h 0))  with
        (t_list,f_list,r_fp) ->
        let new_var_list = 
          let nl = ref [] in
          for i = 0 to List.length var_list -1 do
            if not(List.exists (fun x -> x = i) t_list) && not(List.exists (fun x -> x=i) f_list) then
              nl := !nl @ [List.nth var_list i]
          done;
          !nl 
        in (
          List.map (List.nth var_list) t_list,
          List.map (List.nth var_list) f_list,
          r_fp,new_var_list) 

  let factorise ?(var_list = []) f_p =
    if var_list = [] then factorise_  f_p (Fp.get_all_variables f_p)
    else factorise_ f_p var_list

  let print_factorized_fp = function (t_list,f_list,r_fp,new_var_list) ->  
    print_endline (    
      String.concat "\n" [
        "True variables: ";
        String.concat " " (List.map (fun x -> Elt.convert_to_string (Fp.get_name x))  t_list);
        "False varialbes: ";
        String.concat " " (List.map (fun x -> Elt.convert_to_string (Fp.get_name x))  f_list);
        "Irreducible propositional expression: ";
        string_of_bdd (bdd_of_fp_ r_fp new_var_list)
      ])

  let number_of_solution = function
    | Trivial (l,true) -> 1 lsl (List.length l)
    | Trivial (_,false) -> 0
    | NonTrivial (l,h) ->
      let len = List.length l in
      let module Sd = Standard_data in
      let rec aux data_p i = 
        let true_index = Sd.next_true data_p in
        let false_index= Sd.next_false data_p in
        nb_solution true_index i + nb_solution false_index i
      and nb_solution ind i =
        match ind  with
        | Index.T -> 1 lsl (len - i)
        | Index.F -> 0
        | Index.I x -> aux (Hashtbl.find h x) i+1
      in aux (Hashtbl.find h 0) 0

  let one_solution = function
    | Trivial (_,b) -> b,[]
    | NonTrivial (_,h) ->
      let tlist = ref [] in
      let module Sd = Standard_data in
      let rec aux data_p = 
        let true_index = Sd.next_true data_p in
        if has_solution true_index then 
          begin 
            tlist:=Sd.label data_p::!tlist; 
            true 
          end
        else let false_index= Sd.next_false data_p in
          has_solution false_index
      and has_solution ind  =
        match ind  with
        | Index.T -> true
        | Index.F -> false
        | Index.I x -> aux (Hashtbl.find h x) 
      in aux (Hashtbl.find h 0),!tlist

  let dump_ f_p var_list = print_endline (string_of_bdd (bdd_of_fp_ f_p var_list))

  let dump_fp ?(var_list=[]) f_p =
    if var_list = [] then dump_ f_p (Fp.get_all_variables f_p) else
      dump_ f_p var_list

  let dump_s s = 
    let (f_p,var_list)= Fp.fp_of_string s in dump_ f_p var_list

  let is_satisfiable = function
    |Trivial (_,false) -> false
    | _ -> true 
  let is_valid = function
    |Trivial (_,true) -> true
    | _ -> false
end