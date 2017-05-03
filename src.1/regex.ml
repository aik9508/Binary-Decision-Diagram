open Fp

module ConvertStr:(LabelType with type t = string)=
struct
  type t = string
  let convert_to_string s = s
  let convert_to_t s = s
end

module Str_fp = Make_Fp(ConvertStr)

open Str_fp

let rm_parenthesis str_fp =
  let r = Str.regexp "(\\([][A-Za-z0-9<=>\\| &~]+\\))" in
  let temporal_variable_name = ref 0 in
  let separate_fp = ref [] in
  let rec reduce str_fp =
    let current_str_fp = ref str_fp in
    let rec aux str_fp =
      begin
        try 
          let a = Str.search_forward r str_fp 0 in
          assert (a>=0);
          let substr_without_parenthesis= Str.matched_group 1 str_fp in
          let substr_with_parenthesis = Str.matched_string str_fp in
          let r_substr = Str.regexp_string substr_with_parenthesis in
          current_str_fp := Str.global_replace r_substr ("["^string_of_int !temporal_variable_name^"]") !current_str_fp;
          (*print_string !current_str_fp;*)
          temporal_variable_name:=!temporal_variable_name +1 ;
          reduce substr_without_parenthesis ;
          aux !current_str_fp
        with _ -> 
            separate_fp := !separate_fp @ [!current_str_fp]
      end
    in aux str_fp 
  in reduce str_fp;
  !separate_fp

  let fp3 = "a&&b||(c=>(d<=>e))&&(~f||~g)&&(a=>b)||h<=>i&&j"

  let a = fp_of_string fp3;; 

