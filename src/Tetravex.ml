open Lt
open Bdd_Main
module CS = Core

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

type relative_position = Right|Down
exception NoSolution
let create_var_array width height =
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

let var_to_list var_array=
  CS.Array.fold 
    ~init:[] 
    ~f:(fun l x -> 
        l @ (CS.Array.fold 
               ~init:[] 
               ~f:(fun l y -> 
                   l @ (Array.to_list y)) 
               x)) 
    var_array 

let make_tetravex (file:string) =
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
      | [top;bottom;left;right] -> pieces.(i)<-{top = int_of_string top;
                                                left = int_of_string left;
                                                bottom = int_of_string bottom;
                                                right = int_of_string right}
      | _ -> raise (BDDException "Invalid input data") 
    done;
    let var_array = create_var_array w h in
    let var_list = var_to_list var_array in
    let index_list = List.mapi (fun i x -> i+x ) (Array.to_list (Array.make (w*h) 0)) in
    object (self)
      val width = w
      val height = h
      val pieces = pieces
      val var_array = var_array
      val var_list = var_list
      val index_list = index_list

      method private valid_position k1 k2 rp =
        match rp with
        |Right -> pieces.(k1).right = pieces.(k2).left
        |Down -> pieces.(k1).bottom = pieces.(k2).top 

      method private create_fp_pair i j rp =
        let res = ref (Fp.fp_false()) in
        for p = 0 to width*height-1 do
          for q = 0 to width*height-1 do
            if p<>q && self#valid_position p q rp then
              let (i',j')=match rp with Right -> (i+1,j)|Down -> (i,j+1) in
              res := Fp.fp_or !res (Fp.fp_and (Fp.fp_variable var_array.(i).(j).(p)) (Fp.fp_variable var_array.(i').(j').(q)))
          done
        done;
        !res 

      method private create_fp_pairs () = 
        let res = ref (Fp.fp_true()) in
        for i = 0 to width-1 do
          for j = 0 to height-1 do
            if i<width-1 then
              res := Fp.fp_and !res (self#create_fp_pair i j Right);
            if j<height-1 then
              res := Fp.fp_and !res (self#create_fp_pair i j Down);
          done
        done;
        !res 

      method private create_fp_one_case_per_piece_ k =
        let l=ref[] in
        for i = 0 to width-1 do
          for j=0 to height-1 do
            l:= !l @ [Fp.fp_variable var_array.(i).(j).(k)]
          done
        done;
        or_concat (List.map (one_true_concat !l) index_list) 

      method private create_fp_one_case_per_piece() =
        and_concat (List.map self#create_fp_one_case_per_piece_ index_list) 

      method private create_fp_one_piece_per_case_ i j =
        (*or_concat (List.map (fun x -> Fp.fp_variable var_array.(i).(j).(x)) index_list)*)
        or_concat (List.map (one_true_concat (List.map (fun x -> Fp.fp_variable var_array.(i).(j).(x)) index_list)) index_list)

      method private create_fp_one_piece_per_case () =
        let l= ref [] in
        for i = 0 to width-1 do
          for j= 0 to height - 1 do
            l:=!l @ [self#create_fp_one_piece_per_case_ i j]
          done
        done;
        and_concat !l 

      method create_fp () =
        Fp.fp_and (self#create_fp_pairs()) ( Fp.fp_and (self#create_fp_one_case_per_piece()) (self#create_fp_one_piece_per_case())) 

      method private write_solution file solution =
        let file = "data/"^file in
        let oc = open_out file in
        Printf.fprintf oc "%d %d\n" width height;
        let find i j l =
          let rec find_ = function
            | [] -> []
            | hd::tl -> 
              if hd.i = i && hd.j = j then 
                let p = pieces.(hd.k) in
                Printf.fprintf oc "%d %d %d %d\n" p.top p.bottom p.left p.right;
                tl
              else
                hd :: find_ tl
          in find_ l in
        let l = ref solution in
        for j = 0 to height - 1 do
          for i = 0 to width - 1 do 
            l := find i j !l
          done
        done;
        close_out oc

      method solve file =
        let f_p = self#create_fp() in
        (*print_endline (Fp.string_of_fp f_p);*)
        let b = bdd_of_fp ~var_list:var_list f_p in
        print_endline (string_of_bdd b);
        let has_solution,solution = one_solution b in
        if has_solution then
            self#write_solution file solution
        else
          raise NoSolution
    end
  with End_of_file ->
    raise (BDDException "Invalid input data") 

let solve file =
  let ttx = make_tetravex file in
  let strlist= String.split_on_char '/' file in
  let filename = List.nth strlist ((List.length strlist) -1) in
  let n = String.index filename '.' in
  let filename = String.sub filename 0 n in
  ttx#solve (filename ^ "_solution.txt")
