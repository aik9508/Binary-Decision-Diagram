open Printf

module type LabelType =
sig
  type t
  val convert_to_string : t->string
  val convert_to_t : string -> t
end

module BddGraph = functor (Elt : LabelType) ->
struct
  type t = Elt.t

  type node =
    |T
    |F
    |Node of node * t * node

  type graph =
    |Bdd of node ref
    |Nil

  type index =
    | I of int
    | DT
    | DF

  type data_piece = {
    label: t;
    next_true: index;
    next_false: index;
  }

  type helperMap = (int , data_piece) Hashtbl.t

  exception Incorrect_data_format

  let is_int s =
    try ignore (int_of_string s); true
    with _ -> false

  let index_of_string s = 
    match s with 
    |"@t" -> DT
    |"@f" -> DF
    |s -> if is_int s then I (int_of_string s) else raise  Incorrect_data_format

  let string_of_index ind =
    match ind with
    | DT -> "@t"
    | DF -> "@f"
    | I i -> string_of_int i

  let string_of_data_piece dp =
    Elt.convert_to_string dp.label ^ " " 
    ^ string_of_index dp.next_true ^ " " 
    ^ string_of_index dp.next_false 

  let cons_htb_from_file (file:string) =
    let h:helperMap = Hashtbl.create 10 in
    begin
      let ic = open_in file in
      try
        while true do
          let line = input_line ic in 
          let line_data_str = String.split_on_char ' ' line in
          let line_data =
            match line_data_str with
            |[key;label;next_true;next_false]-> assert (is_int key); 
              (int_of_string key, {label=Elt.convert_to_t label; 
                                   next_true = index_of_string next_true; 
                                   next_false = index_of_string next_false})
            | _ -> raise Incorrect_data_format in
          Hashtbl.add h (fst line_data) (snd line_data);
          print_int (fst line_data);
          print_string " ";
          print_string ((string_of_data_piece (snd line_data)) ^ "\n")
        done
      with End_of_file ->
        close_in_noerr ic
    end;
    h

  let cons_from_file (file:string) =
    let h = cons_htb_from_file file in
    let rec get_node ind = 
      match ind with
      |DT->T
      |DF->F
      |I i -> aux (Hashtbl.find h i) 
    and  aux dp =
      let node_true = get_node dp.next_true in
      let node_false = get_node dp.next_false in
      Node (node_true , dp.label , node_false) 
    in Bdd (ref (aux (Hashtbl.find h 0)))
end


module ConvertStr =
struct
  type t = string
  let convert_to_string s = s
  let convert_to_t s = s
end

module StrBddGraph = BddGraph(ConvertStr)

(*let h = StrBddGraph.cons_htb_from_file("src/data.txt");;

print_string (StrBddGraph.string_of_data_piece (Hashtbl.find h 1))*)

let g=StrBddGraph.cons_from_file("src/data.txt");;