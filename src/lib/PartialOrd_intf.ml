module type Minimal = sig
  type t

  val leq : t -> t -> bool
end

module type S = sig
  include Minimal

  val geq : t -> t -> bool
  val eq : t -> t -> bool
  val neq : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val compare_opt : t -> t -> int option
end

module type PartialOrd = sig
  module type Minimal = Minimal
  module type S = S

  module Make (X : Minimal) : S with type t := X.t
  module MakeTuple2 (X : Minimal) (Y : Minimal) : S with type t := X.t * Y.t
end
