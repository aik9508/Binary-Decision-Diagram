open Fp
open Lt

module ConvertStr:(LabelType with type t = string)=
struct
  type t = string
  let convert_to_string s = s
  let convert_to_t s = s
  let hash x = Hashtbl.hash x
  let equal x y = x = y
end


module Make_bdd(Elt:LabelType) =
struct
  module Fp = Make_Fp(Elt)
  type t = Elt.t
  type fp = Fp.fp
  type variable = Fp.variable
  type node =T|F|Node of node * t * node
  exception BDDException of string

  module Index = struct
    type t = I of int | DT | DF
    let hash x = match x with I i -> Hashtbl.hash i | DT -> -1 | DF -> -3
    let equal x y = match x,y with (DT,DT)|(DF,DF) -> true | (I i1, I i2)-> i1=i2 | _ ->false 
    let index_true() = DT
    let index_false()  = DF
    let isTure x = x = DT
    let isFalse x = x = DF
    let to_string x = match x with I i -> string_of_int i| DT -> "@t"| DF -> "@f"
    let to_index s = match s with "@t" -> DT | "@f"-> DF | s -> I (int_of_string s)
    let to_int x = match x with I i -> i | DT -> -1 | DF -> -3
    let index i = I i
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
    let next_ture x= x.nt
    let next_false x = x.nf
    let label x = x.label
    let to_string x = String.concat " " [Elt.convert_to_string x.label; Index.to_string x.nt ; Index.to_string x.nf]
    let to_std_data s = 
      let l = String.split_on_char ' ' s in
      match l with
      | [label;nt;nf] -> {label=Elt.convert_to_t label; nt = Index.to_index nt ; nf= Index.to_index nf}
      | _ -> raise (BDDException "Invalid input format")
    let std_data label nt nf = {label;nt;nf}
  end
  type data_piece = Standard_data.t

  module Ht = Hashtbl.Make(Standard_data)
  exception Unmatched_arguments
  let get_graph (f_p:fp) (var_list:variable list) =
    let rec aux f_p var_list =
      if Fp.is_fp_true f_p then T else if Fp.is_fp_false f_p then F else
        begin
          match var_list with
          |[] -> 
            if Fp.is_fp_true f_p then T 
            else if Fp.is_fp_false f_p then F 
            else raise Unmatched_arguments
          | hd :: tl -> 
            begin
              let var_pos = Fp.get_var_position f_p hd in
              if Fp.bool_of_tree var_pos then
                let leftFp = Fp.partial_eval f_p (Fp.var_false hd) var_pos in
                let leftNode = aux leftFp tl in
                let rightNode = aux (Fp.partial_eval f_p (Fp.var_true hd) var_pos) tl in
                Node (leftNode , Fp.get_name hd, rightNode)
              else
                aux f_p tl
            end
        end
    in aux f_p var_list

  let rec size_of_node n =
    match n with
    |T|F -> 1
    |Node (leftNode,_,rightNode) -> (max (size_of_node leftNode) (size_of_node rightNode)) + 1 

  let reduce_graph g =
    let hmp = Ht.create ((size_of_node g)*2) in
    let l = ref [] in
    let current_index = ref 0 in
    let rec aux = function
      | T -> Index.index_true()
      | F -> Index.index_false()
      | Node (leftNode,v,rightNode) -> 
        let leftIndex = aux leftNode in
        let rightIndex = aux rightNode in 
        let data_p = Standard_data.std_data v rightIndex leftIndex in
        if not(Ht.mem hmp data_p) then 
          begin
            if Index.equal leftIndex rightIndex then
              if (Index.isTure leftIndex) || (Index.isFalse leftIndex) then
                  leftIndex
              else
                begin
                  Ht.add hmp data_p (Index.to_int leftIndex);
                  leftIndex
                end
            else
              begin
                Ht.add hmp data_p !current_index;
                l := data_p :: !l;
                current_index:=!current_index+1;
                Index.index (!current_index - 1)
              end
          end
        else Index.index (Ht.find hmp data_p) 
    in let r = aux g in
    if Index.isTure r || Index.isFalse r then "" else
      begin
        let max_index = !current_index - 1 in
        let change_index ind = if Index.isTure ind || Index.isFalse ind then ind else Index.index (max_index - Index.to_int ind) in
        let change_data_piece data_p = 
          Standard_data.std_data 
            (Standard_data.label data_p) 
            (change_index (Standard_data.next_ture data_p))
            (change_index (Standard_data.next_false data_p))
        in 
        String.concat "\n" (
          List.mapi (fun i data_p -> (string_of_int i) ^ " " ^ Standard_data.to_string (change_data_piece data_p)) !l
        )
      end

  let number_of_solution g height =
    let rec aux n i =
      match n with 
      |T -> 1 lsl (height-i)
      |F -> 0 
      |Node (leftNode,_,rightNode) -> (aux leftNode (i+1)) + (aux rightNode (i+1)) 
    in aux g 0

  let rec print_node n =
    match n with
    | T -> print_string "T"
    | F -> print_string "F"
    | Node (leftNode,v_name,rightNode) ->
      print_node leftNode;
      print_string (" "^Elt.convert_to_string v_name^" ");
      print_node rightNode

  let cons_htb_from_file (file:string) =
    let h = Hashtbl.create 10 in
    begin
      let ic = open_in file in
      try
        while true do
          let line = input_line ic in 
          let line_data_str = String.split_on_char ' ' line in
          let line_data =
            match line_data_str with
            |[key;label;next_true;next_false]-> 
              (int_of_string key, Standard_data.std_data (Elt.convert_to_t label)
                 (Index.to_index next_true) 
                 (Index.to_index next_false))
            | _ -> raise (BDDException "Incorrect data format") in
          Hashtbl.add h (fst line_data) (snd line_data);
          print_int (fst line_data);
          print_string " ";
          print_string ((Standard_data.to_string (snd line_data)) ^ "\n")
        done
      with End_of_file ->
        close_in_noerr ic
    end;
    h

  let cons_from_file (file:string) =
    let h = cons_htb_from_file file in
    let rec get_node ind = 
      if Index.isTure ind then T else if Index.isFalse ind then F
      else aux (Hashtbl.find h (Index.to_int ind))
    and  aux dp =
      let node_true = get_node (Standard_data.next_ture dp) in
      let node_false = get_node (Standard_data.next_false dp) in
      Node (node_true , Standard_data.label dp , node_false) 
    in aux (Hashtbl.find h 0)
end

module Str_bdd = Make_bdd(ConvertStr)

open Str_bdd.Fp

let str1 = "~(a=>b)"
let str2 = "((a&&a&&(a&&(a||~~~~~~a))))"
let str3 = "a&&b||(c=>(d<=>e))&&(~f||~g)&&(a=>b)||h<=>i&&j"
let str4 = "a&&b&&c||a&&c&&b||b&&a&&c||b&&c&&a||c&&a&&b||c&&b&&a"

let (fp1,var_list) = fp_of_string str4

let f_v = List.nth  var_list 0

let t1 = get_var_position fp1 f_v

let fp2 = partial_eval fp1 (var_false f_v ) t1

let n1 = Str_bdd.get_graph fp2 (List.tl var_list);;

let n = Str_bdd.get_graph fp1 var_list;;

Str_bdd.print_node n;;
print_string "\n";;
print_endline (Str_bdd.reduce_graph n);;

let g=Str_bdd.cons_from_file "data/data.txt";;