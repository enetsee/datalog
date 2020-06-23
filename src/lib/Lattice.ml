(** Bounded meet semi-lattice with top...? *)
module type S = sig
  type t

  val bottom : t
  val top : t
  val meet : t -> t -> t
end
