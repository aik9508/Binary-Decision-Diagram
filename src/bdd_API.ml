open Fp
open Bdd_Main
open Tetravex

module SB = Make_bdd(StrLabel)
module SF = Make_Fp(StrLabel) 

let tetravex filename =
  resolve filename

let create_true = SF.fp_true

let create_false = SF.fp_false

let is_true = SF.is_fp_true

let is_false = SF.is_fp_false

let fp_and = SF.fp_and 

let fp_or = SF.fp_or 

let fp_imp = SF.fp_imp

let fp_equi = SF.fp_equi

let fp_variable = SF.fp_variable

let eval f_p v_name_list bl =
  SF.eval f_p v_name_list ~bl:bl

let eval_s s v_name_list bl =
    let (f_p,_) = SF.fp_of_string s in
    eval f_p v_name_list bl 

let dump_s s =
  SB.dump_s s 

let dump_fp f_p =
  SB.dump_fp f_p

let is_satisfiable_s s =
  SF.is_satisfiable_s s

let is_satisfiable_fp f_p =
  SF.is_satisfiable_fp f_p

let is_valid_s s =
  SF.is_valid_s s

let is_valid_fp f_p =
  SF.is_valid_fp f_p

let fp_of_string = SF.fp_of_string 

