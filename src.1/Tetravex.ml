open Fp
open Bddcycl
open Core.Std

let width = 3
let height = 3

type position = {i:int;j:int;k:int}
type piece = {top:int ; left:int; bottom:int; right:int}

let p1 = {top=5;left=8;bottom=4;right=9}
let p2 = {top=1;left=9;bottom=9;right=5}
let p3 = {top=9;left=6;bottom=6;right=6}
let p4 = {top=4;left=7;bottom=1;right=6}
let p5 = {top=9;left=5;bottom=8;right=1}
let p6 = {top=8;left=6;bottom=4;right=3}
let p7 = {top=1;left=1;bottom=0;right=9}
let p8 = {top=6;left=9;bottom=4;right=0}
let p9 = {top=4;left=0;bottom=2;right=8}

let pieces =[| p1; p2; p3; p4; p5; p6; p7; p8; p9|]

module Tetravex_label:(LabelType with type t = position)=
struct
  type t = position
  let convert_to_string a = string_of_int a.i ^"_"^string_of_int a.j ^"_"^string_of_int a.k
  let convert_to_t s = 
    let l = String.split_on_chars ~on:['_'] s in
    {i=int_of_string (List.nth_exn l 0); 
     j= int_of_string (List.nth_exn l 1); 
     k= int_of_string (List.nth_exn l 2)}
end

module Tetravex_bdd = Make_bddAcycl(Tetravex_label) 
open Tetravex_bdd

(*let create_variables () =
  let nil = BaFp.create_var_false {i=0;j=0;k=0} in
  let a = nil
          |> Array.create ~len:(width*height)
          |> Array.create ~len:height
          |> Array.create ~len:width in
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      for k = 0 to width*height-1 do
        a.(i).(j).(k) <- (BaFp.create_var_false {i=i;j=j;k=k});
        print_endline (Tetravex_label.convert_to_string (BaFp.get_name a.(i).(j).(k)))
      done
    done
  done;
  a

  let variables = create_variables()*)

(*let () = for i = 0 to width-1 do
    for j = 0 to height-1 do
      for k = 0 to width*height-1 do
        print_endline (Tetravex_label.convert_to_string (BaFp.get_name variables.(i).(j).(k)))
      done
    done
  done*)

let rec flatten = function [] -> [] | x :: s -> x @ flatten s

(*let var_list =
  List.map ~f:(fun x -> ref x) 
    (Array.fold 
       ~init:[] 
       ~f:(fun l x -> 
           l @ (Array.fold 
                  ~init:[] 
                  ~f:(fun l y -> 
                      l @ (Array.to_list y)) 
                  x)) 
       variables)*)
let var_list =
  let l = ref [] in
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      for k = 0 to width*height-1 do
        l := !l @ [ BaFp.create_var_false {i=i;j=j;k=k}]
      done
    done
  done;
  !l

let var_list_2 = 
  let l = ref [] in
  for k = 0 to width*height-1 do
    for i = 0 to width-1 do
      for j = 0 to height-1 do
        l := !l @ [ BaFp.create_var_false {i=i;j=j;k=k}]
      done
    done
  done;
  !l

type relative_position = Right|Down

let valid_position k1 k2 rp =
  match rp with
  |Right -> pieces.(k1).right = pieces.(k2).left
  |Down -> pieces.(k1).bottom = pieces.(k2).top
let create_fp_pair i j rp =
  let res = ref (BaFp.fp_false()) in
  for p = 0 to width*height-1 do
    for q = 0 to width*height-1 do
      if p<>q && valid_position p q rp then
        let (i',j')=match rp with Right -> (i+1,j)|Down -> (i,j+1) in
        res := BaFp.fp_or !res (BaFp.fp_and (BaFp.fp_variable (List.nth_exn var_list ((i*height+j)*width*height+p))) (BaFp.fp_variable (List.nth_exn var_list ((i'*height+j')*width*height+q))))
    done
  done;
  !res

let create_fp_pairs () = 
  let res = ref (BaFp.fp_true()) in
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      if i<width-1 then
        res := BaFp.fp_and !res (create_fp_pair i j Right);
      if j<height-1 then
        res := BaFp.fp_and !res (create_fp_pair i j Down);
    done
  done;
  !res

let rec and_concat = function
  | [] -> BaFp.fp_true()
  | x :: s -> BaFp.fp_and x (and_concat s)

let rec or_concat = function
  | [] -> BaFp.fp_false()
  | x::s -> BaFp.fp_or x (or_concat s)

let one_true_concat l ind=
  let rec aux i l =
    match l with
    |[] -> BaFp.fp_true()
    | x::s -> if i = ind then BaFp.fp_and x (aux (i+1) s) else BaFp.fp_and (BaFp.fp_not x) (aux (i+1) s) 
  in aux 0 l

let index_list = List.mapi ~f:(fun i x -> i+x ) (Array.to_list (Array.create ~len:(width*height) 0))

let create_fp_one_case_per_piece_ k =
  let l=ref[] in
  for i = 0 to width-1 do
    for j=0 to height-1 do
      l:= !l @ [BaFp.fp_variable (List.nth_exn var_list ((i*height+j)*width*height+k))]
    done
  done;
  or_concat (List.map ~f:(one_true_concat !l) index_list)

let create_fp_one_case_per_piece() =
  and_concat (List.map ~f:create_fp_one_case_per_piece_ index_list)

let create_fp_one_piece_per_case_ i j =
  or_concat (List.map ~f:(one_true_concat (List.map ~f:(fun x -> BaFp.fp_variable (List.nth_exn var_list ((i*height+j)*width*height+x))) index_list)) index_list)

let create_fp_one_piece_per_case () =
  let l= ref [] in
  for i = 0 to width-1 do
    for j= 0 to height - 1 do
      l:=!l @ [create_fp_one_piece_per_case_ i j]
    done
  done;
  and_concat !l

let create_fp () =
  BaFp.fp_and (create_fp_pairs()) ( BaFp.fp_and (create_fp_one_case_per_piece()) (create_fp_one_piece_per_case())) 

let () =
  let n_var = List.length var_list in
  List.iter ~f:(fun x -> print_endline (Tetravex_label.convert_to_string (BaFp.get_name x))) var_list ;
  let f_p = create_fp() in
  print_endline (BaFp.string_of_fp f_p);
  print_endline "";
  let n = get_graph f_p var_list in
  print_int (Tetravex_bdd.number_of_solution n n_var);
  print_endline "";
  print_endline (reduce_graph n)
