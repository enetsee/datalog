(* Elements that can be translated to a representation in `Core` *)
module type S = sig
  type t
  type repr

  module Make (M : SourceM.S) : sig
    val to_core : t -> repr M.t
  end
end
