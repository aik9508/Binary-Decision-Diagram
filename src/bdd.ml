open Fp
open Bdd_Main
open Tetravex

module SB = Make_bdd(StrLabel)
module SF = SB.Fp

let tetravex filename =
  solve filename

let dump s =
  SB.dump_s s 

let is_satisfiable s =
  SF.is_satisfiable_s s

let is_valid s =
  SF.is_valid_s s

let test () =
  let argv = Sys.argv in
  match argv.(1) with
  |"dump" -> if Array.length argv >=3 then dump argv.(2)
  |"valid" -> if Array.length argv >=3 then 
      print_endline (string_of_bool (is_valid argv.(2)))
  |"satisfiable" -> if Array.length argv >=3 then 
      print_endline (string_of_bool (is_satisfiable argv.(2)))
  |"tetravex" -> if Array.length argv >=3 then tetravex argv.(2)
  | _ -> exit 1

let () = test()