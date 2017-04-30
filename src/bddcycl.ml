open Fp
open Core.Std

module ConvertStr:(LabelType with type t = string)=
struct
  type t = string
  let convert_to_string s = s
  let convert_to_t s = s
end


module Make_bddAcycl(Elt:LabelType) =
struct
  module BaFp = Make_Fp(Elt)
  type t = Elt.t
  type fp = BaFp.fp
  type variable = BaFp.variable

  type node =T|F|Node of node * t * node
  type graph =Bdd of node ref|Nil
  type index = I of int| DT| DF
  type data_piece = {
    label: t;
    next_true: index;
    next_false: index;
  }
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

  exception Unmatched_arguments

  let get_graph (f_p:fp) (var_list:variable list) =
    let rec aux f_p var_list =
      if BaFp.is_fp_true f_p then T else if BaFp.is_fp_false f_p then F else
        begin
          match var_list with
          |[] -> 
            if BaFp.is_fp_true f_p then 
              T 
            else if BaFp.is_fp_false f_p then 
              F 
            else 
              (print_string (BaFp.string_of_fp f_p); 
               raise Unmatched_arguments)
          | hd :: tl -> 
            begin
              let var_pos = BaFp.get_var_position f_p hd in
              print_string (Elt.convert_to_string (BaFp.get_name hd));
              if BaFp.bool_of_tree var_pos then
                let leftFp = BaFp.partial_eval f_p (BaFp.var_false hd) var_pos in
                print_string (BaFp.string_of_fp leftFp);
                print_string "\n";
                let leftNode = aux leftFp tl in
                let rightNode = aux (BaFp.partial_eval f_p (BaFp.var_true hd) var_pos) tl in
                Node (leftNode , BaFp.get_name hd, rightNode)
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
    let hmp = String.Table.create() ~size:(size_of_node g) in
    let l = ref [] in
    let current_index = ref 0 in
    let rec aux = function
      | T -> DT
      | F -> DF
      | Node (leftNode,v,rightNode) -> 
        let leftIndex = aux leftNode in
        let rightIndex = aux rightNode in 
        let data_p = {label=v; next_true = rightIndex; next_false = leftIndex} in
        let s = string_of_data_piece data_p in
        if not(Hashtbl.mem hmp s) then 
          begin
            Hashtbl.add_exn hmp ~key:s ~data:!current_index;
            l := data_p :: !l;
            current_index:=!current_index+1;
            I (!current_index - 1)
          end
        else let ind = Hashtbl.find_exn hmp s in I ind
    in match aux g with 
    | DT|DF -> ""
    |_->
      begin
        let max_index = !current_index - 1 in
        let change_index = function
          | I i -> I (max_index-i)
          | a -> a in
        let change_data_piece = 
          fun data_p -> {data_p with next_true = (change_index data_p.next_true) ; next_false = (change_index data_p.next_false)} 
        in 
        !l
        |> List.mapi ~f:(fun i data_p -> (string_of_int i)^" "^string_of_data_piece (change_data_piece data_p))
        |> String.concat ~sep:"\n"
      end

  let rec print_node n =
    match n with
    | T -> print_string "T"
    | F -> print_string "F"
    | Node (leftNode,v_name,rightNode) ->
      print_node leftNode;
      print_string (" "^Elt.convert_to_string v_name^" ");
      print_node rightNode
end

module Str_bddAcycl = Make_bddAcycl(ConvertStr)

open Str_bddAcycl.BaFp

let str1 = "~(a=>b)"
let str2 = "((a&&a&&(a&&(a||~~~~~~a))))"
let str3 = "a&&b||(c=>(d<=>e))&&(~f||~g)&&(a=>b)||h<=>i&&j"

let (fp1,var_list) = fp_of_string str2

let f_v = List.nth_exn  var_list 0

let t1 = get_var_position fp1 f_v

let fp2 = partial_eval fp1 {f_v with value=false} t1

let n1 = Str_bddAcycl.get_graph fp2 (List.tl_exn var_list);;

let n = Str_bddAcycl.get_graph fp1 var_list;;

Str_bddAcycl.print_node n;;
print_string "\n";;
print_endline (Str_bddAcycl.reduce_graph n);;