module type LabelType =
  sig
    type t
    val convert_to_string : t -> string
    val convert_to_t : string -> t
  end
module type FormulePropositionnelle =
  sig
    type t
    type variable = { name : t; value : bool; }
    val var_false : variable -> variable
    val var_true : variable -> variable
    val get_value : variable -> bool
    val get_name : variable -> t
    type fp = V of variable | T | F | Not of fp | Comp of fp * connector * fp
    and connector = C_and | C_or | C_imp | C_equi
    val is_fp_true : fp -> bool
    val is_fp_false : fp->bool
    type tree = TT | TF | TN of tree * tree
    val bool_of_tree : tree->bool
    val get_var_position : fp -> variable -> tree
    val partial_eval : fp -> variable -> tree -> fp
    val fval : fp -> fp
  end
module Make_Fp :
  functor (Elt : LabelType) -> FormulePropositionnelle with type t = Elt.t
