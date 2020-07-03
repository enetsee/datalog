(* Elements that can be translated to a representation in `Core` *)
module type S = sig
  type t
  type repr

  val to_core : t -> (repr, Err.t) result
end
