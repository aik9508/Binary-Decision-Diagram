open Bdd_API

let test1 () = 
    print_endline "Resolve a 2*2 tetravex problem";
    tetravex "data/tetravex2.txt" 

let test2 () = 
    print_endline "Construct a propositional expression from a standard input";
    print_endline "The expression to construct: '(a&&~b)||c=>(d<=>e)'";
    print_string "The expression constructed : ";
    print_endline (string_of_fp (fst (fp_of_string "(a&&~b)||c=>(d<=>e)")))

let test3 () =
    print_endline "A potential interpretation of a propositional expression : ";
    print_endline "The expression to evaluate : '(a&&~b)||c=>(d<=>e)'";
    print_endline "With a=false b=true c= true d=false e=false";
    print_endline "The expected result : true";
    print_string "The result evaluated : " ;
    print_endline (string_of_bool (eval_s "(a&&~b)||c=>(d<=>e)" ["a";"b";"c";"d";"e"] [false;true;true;false;false]));
    print_endline "The expression to evaluate : '(a&&~b)||c=>(d<=>e)'";
    print_endline "With a=false b=true c= true d=false e=true";
    print_endline "The expected result : false";
    print_string "The result evaluated : " ;
    print_endline (string_of_bool (eval_s "(a&&~b)||c=>(d<=>e)" ["a";"b";"c";"d";"e"] [false;true;true;false;true]))
    
let test4 () =
    print_endline "Check if a propositional expression is satisfaible : ";
    print_endline "The expression to check : '(a&&~b)||c=>(d<=>e)'";
    print_endline "The expected result : true";
    print_string "The result computed : " ;
    print_endline (string_of_bool (is_satisfiable_s "(a&&~b)||c=>(d<=>e)"));
    print_endline "The expression to check : 'a&&~a'";
    print_endline "The expected result : false";
    print_string "The result computed : " ;
    print_endline (string_of_bool (is_satisfiable_s "a&&~a"))

let test5() = 
    print_endline "Check if a propositional expression is valid : ";
    print_endline "The expression to check : '(a&&~b)||c=>(d<=>e)'";
    print_endline "The expected result : false";
    print_string "The result computed : " ;
    print_endline (string_of_bool (is_valid_s "(a&&~b)||c=>(d<=>e)"));
    print_endline "The expression to check : '(a<=>b)||(a<=>~b)'";
    print_endline "The expected result : true";
    print_string "The result computed : " ;
    print_endline (string_of_bool (is_satisfiable_s "(a<=>b)||(a<=>~b)"))

let test6() = 
    print_endline "Create a binary decision tree from a propositional expression : ";
    print_endline "The expression to transform : '(a&&~b)||c=>(d<=>e)' ";
    print_endline "Result : ";
    dump_s "(a&&~b)||c=>(d<=>e)"

let test7() =
    print_endline "Read a binary decision tree from a file : ";
    print_endline (string_of_bdd (bdd_from_file "data/bdd1.txt"))

let test8() = 
    print_endline "Factorise a propositional expression : ";
    print_endline "Given an expression, this function will find all variables that need to be true or false to ensure the value of the expression is true. The expression to factorise : '(a&&b)&&(~c||~d)&&~e' We can find that if this expression is true, then a and b are necessarily true and e should be false";
    print_endline "Result : ";
    let (f_p, var_list) = fp_of_string "(a&&b)&&(~c||~d)&&~e" in
    print_factorized_fp (factorise ~var_list:var_list f_p )

let () = test7()
    