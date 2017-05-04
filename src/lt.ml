module type Hashable = sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

module type LabelType =
sig
  type t
  val convert_to_string : t->string
  val convert_to_t : string -> t
  include Hashable with type t := t
end