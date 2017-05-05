open Lt
open Bdd
module CS = Core.Std

type position = {i:int;j:int;k:int}
type piece = {top:int ; left:int; bottom:int; right:int}

module Tetravex_label:(LabelType with type t = position)=
struct
  type t = position
  let convert_to_string a = string_of_int a.i ^"_"^string_of_int a.j ^"_"^string_of_int a.k
  let convert_to_t s = 
    let l = String.split_on_char '_' s in
    {i=int_of_string (List.nth l 0); 
     j= int_of_string (List.nth l 1); 
     k= int_of_string (List.nth l 2)}
  let hash p = ((p.i * 13 + p.j) * 59 +p.k )*79
  let equal p1 p2 = p1.i = p2.i && p1.j = p2.j && p1.k = p2.k
end

module TB = Make_bdd(Tetravex_label) 
open TB

let make_tetravex(file:string) =
  let ic = open_in file in
  try
    let line = input_line ic in
    let wh = String.split_on_char ' ' line in 
    let (w,h) =
      match wh with
      | [w;h] -> (int_of_string w, int_of_string h)
      | _ ->  raise (BDDException "Invalid input data") 
    in let init = {top=0;left=0;bottom=0;right=0} in
    let pieces = Array.make (w*h) init in
    for i = 0 to w*h-1 do
      let line = String.split_on_char ' ' (input_line ic) in
      match line with
      | [top;left;bottom;right] -> pieces.(i)<-{top = int_of_string top;
                                                left = int_of_string left;
                                                bottom = int_of_string bottom;
                                                right = int_of_string right}
      | _ -> raise (BDDException "Invalid input data") 
    done;
    ((w,h),pieces)
  with End_of_file ->
    raise (BDDException "Invalid input data") 

let ((width,height),pieces) = make_tetravex("data/tetravex2.txt")

let create_variables () =
  let nil = Fp.create_var_false {i=0;j=0;k=0} in
  let a' = Array.make_matrix width height nil in
  let a = Array.map (Array.map (fun x -> Array.make (width*height) x)) a' in
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      for k = 0 to width*height-1 do
        a.(i).(j).(k) <- (Fp.create_var_false {i=i;j=j;k=k})
      done
    done
  done;
  a

let variables = create_variables()

let var_list =
  CS.Array.fold 
    ~init:[] 
    ~f:(fun l x -> 
        l @ (CS.Array.fold 
               ~init:[] 
               ~f:(fun l y -> 
                   l @ (Array.to_list y)) 
               x)) 
    variables

type relative_position = Right|Down

let valid_position k1 k2 rp =
  match rp with
  |Right -> pieces.(k1).right = pieces.(k2).left
  |Down -> pieces.(k1).bottom = pieces.(k2).top
let create_fp_pair i j rp =
  let res = ref (Fp.fp_false()) in
  for p = 0 to width*height-1 do
    for q = 0 to width*height-1 do
      if p<>q && valid_position p q rp then
        let (i',j')=match rp with Right -> (i+1,j)|Down -> (i,j+1) in
        res := Fp.fp_or !res (Fp.fp_and (Fp.fp_variable variables.(i).(j).(p)) (Fp.fp_variable variables.(i').(j').(q)))
    done
  done;
  !res

let create_fp_pairs () = 
  let res = ref (Fp.fp_true()) in
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      if i<width-1 then
        res := Fp.fp_and !res (create_fp_pair i j Right);
      if j<height-1 then
        res := Fp.fp_and !res (create_fp_pair i j Down);
    done
  done;
  !res

let rec and_concat = function
  | [] -> Fp.fp_true()
  | x :: s -> Fp.fp_and x (and_concat s)

let rec or_concat = function
  | [] -> Fp.fp_false()
  | x::s -> Fp.fp_or x (or_concat s)

let one_true_concat l ind=
  let rec aux i l =
    match l with
    |[] -> Fp.fp_true()
    | x::s -> if i = ind then Fp.fp_and x (aux (i+1) s) else Fp.fp_and (Fp.fp_not x) (aux (i+1) s) 
  in aux 0 l

let index_list = List.mapi (fun i x -> i+x ) (Array.to_list (Array.make (width*height) 0))

let create_fp_one_case_per_piece_ k =
  let l=ref[] in
  for i = 0 to width-1 do
    for j=0 to height-1 do
      l:= !l @ [Fp.fp_variable variables.(i).(j).(k)]
    done
  done;
  or_concat (List.map (one_true_concat !l) index_list)

let create_fp_one_case_per_piece() =
  and_concat (List.map create_fp_one_case_per_piece_ index_list)

let create_fp_one_piece_per_case_ i j =
  or_concat (List.map (one_true_concat (List.map (fun x -> Fp.fp_variable variables.(i).(j).(x)) index_list)) index_list)

let create_fp_one_piece_per_case () =
  let l= ref [] in
  for i = 0 to width-1 do
    for j= 0 to height - 1 do
      l:=!l @ [create_fp_one_piece_per_case_ i j]
    done
  done;
  and_concat !l

let create_fp () =
  Fp.fp_and (create_fp_pairs()) ( Fp.fp_and (create_fp_one_case_per_piece()) (create_fp_one_piece_per_case())) 

let () =
  let n_var = List.length var_list in
  (*List.iter (fun x -> print_endline (Tetravex_label.convert_to_string (Fp.get_name x))) var_list ;*)
  let f_p = create_fp_one_case_per_piece() in
  (*print_endline (Fp.string_of_fp f_p);
    print_endline "";*)
  (*let fp1 = Fp.partial_eval_i f_p var_list 0 false in
  print_endline (Fp.string_of_fp fp1);
  print_endline "";
  print_endline "";
  print_endline "";*)
  (*let fp2 = Fp.partial_eval_i fp1 var_list 1 true in
    print_endline (Fp.string_of_fp fp2);
    print_endline "";*)
  (*let fp3 = Fp.partial_eval_i fp1 var_list 1 false in
    print_endline (Fp.string_of_fp fp3);
    print_endline "";*)
  let n = get_graph f_p var_list in
  print_int (number_of_solution n n_var);
  print_endline "";
  print_endline (reduce_graph n);
  print_endline (print_factorized_fp (factorise f_p var_list))

