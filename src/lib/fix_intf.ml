module type S = sig
  module F : Functor.S1

  type t = In of t F.t

  val embed : t F.t -> t

  include Proj.S with module F := F and type t := t
  include Recursive.S with module F := F and type t := t
  include Corecursive.S with module F := F and type t := t
end

module type S2 = sig
  module F : Functor.S2

  type 'a t = In of ('a t, 'a) F.t

  val embed : ('a t, 'a) F.t -> 'a t

  include Proj.S2 with module F := F and type 'a t := 'a t
  include Recursive.S2 with module F := F and type 'a t := 'a t
  include Corecursive.S2 with module F := F and type 'a t := 'a t
end

module type Fix = sig
  module type S = S
  module type S2 = S2

  module Make (F : Functor.S1) : S with module F := F
  module Make2 (F : Functor.S2) : S2 with module F := F
end
