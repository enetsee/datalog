module type Minimal = sig
  module F : Functor.S1

  type t

  val embed : t F.t -> t
  val proj : t -> t F.t
end

module type Minimal2 = sig
  module F : Functor.S2

  type 'a t

  val embed : ('a t, 'a) F.t -> 'a t
  val proj : 'a t -> ('a t, 'a) F.t
end

module type S = sig
  type t

  module F : Functor.S1

  type 'a coalgebra = 'a -> 'a F.t

  val ana : ('a -> 'a F.t) -> 'a -> t
  val transform_top_down : (t -> t) -> t -> t

  type 'a r_coalgebra = 'a -> (t, 'a) result F.t

  val apo : ('a -> (t, 'a) result F.t) -> 'a -> t
  val transform_partial : (t -> (t, t) result) -> t -> t
end

module type S2 = sig
  type 'a t

  module F : Functor.S2

  type ('a, 'e) coalgebra = 'a -> ('a, 'e) F.t

  val ana : ('a -> ('a, 'e) F.t) -> 'a -> 'e t
  val transform_top_down : ('a t -> 'a t) -> 'a t -> 'a t

  type ('a, 'e) r_coalgebra = 'a -> (('e t, 'a) result, 'e) F.t

  val apo : ('a -> (('e t, 'a) result, 'e) F.t) -> 'a -> 'e t
  val transform_partial : ('a t -> ('a t, 'a t) result) -> 'a t -> 'a t
end

module type Corecursive = sig
  module type Minimal = Minimal
  module type Minimal2 = Minimal2
  module type S = S
  module type S2 = S2

  module Make (X : Minimal) : S with module F := X.F and type t := X.t
  module Make2 (X : Minimal2) : S2 with module F := X.F and type 'a t := 'a X.t
end
