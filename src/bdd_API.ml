open Fp
open Bdd_Main
open Tetravex

module SB = Make_bdd(StrLabel)
module SF = SB.Fp

(*Tetravex*)

let tetravex filename =
  resolve filename

(*logical operations*)

let create_true = SF.fp_true

let create_false = SF.fp_false

let is_true = SF.is_fp_true

let is_false = SF.is_fp_false

let fp_and = SF.fp_and 

let fp_or = SF.fp_or 

let fp_not = SF.fp_not

let fp_imp = SF.fp_imp

let fp_equi = SF.fp_equi

let fp_variable = SF.fp_variable

let eval f_p v_name_list bl =
  SF.eval f_p v_name_list ~bl:bl

let eval_s s v_name_list bl =
    let (f_p,_) = SF.fp_of_string s in
    eval f_p v_name_list bl 

let is_satisfiable_s = SF.is_satisfiable_s 

let is_satisfiable_fp = SF.is_satisfiable_fp 

let is_valid_s = SF.is_valid_s

let is_valid_fp = SF.is_valid_fp

let fp_of_string = SF.fp_of_string 

let string_of_fp = SF.string_of_fp

(*Interface for bdd_main*)

let dump_s s = SB.dump_s s 

let dump_fp f_p = SB.dump_fp f_p

let bdd_from_file = SB.cons_from_file

let bdd_of_fp f_p = SB.get_graph f_p

let fp_from_file = SB.fp_from_file

let fp_of_bdd = SB.fp_of_bdd 

let string_of_bdd = SB.reduce_graph 

let factorise = SB.factorise

let print_factorized_fp = SB.print_factorized_fp

