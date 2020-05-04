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

  type 'a algebra = 'a F.t -> 'a

  val cata : ('a F.t -> 'a) -> t -> 'a
  val transform_bottom_up : (t -> t) -> t -> t

  type 'a r_algebra = t -> 'a F.t -> 'a

  val para : (t -> 'a F.t -> 'a) -> t -> 'a
  val transform_with_context : (t -> t -> t) -> t -> t
end

module type S2 = sig
  type 'a t

  module F : Functor.S2

  type ('a, 'e) algebra = ('a, 'e) F.t -> 'a

  val cata : (('a, 'e) F.t -> 'a) -> 'e t -> 'a
  val transform_bottom_up : ('a t -> 'a t) -> 'a t -> 'a t

  type ('a, 'e) r_algebra = 'e t -> ('a, 'e) F.t -> 'a

  val para : ('e t -> ('a, 'e) F.t -> 'a) -> 'e t -> 'a
  val transform_with_context : ('a t -> 'a t -> 'a t) -> 'a t -> 'a t
end

module type Recursive = sig
  module type Minimal = Minimal
  module type Minimal2 = Minimal2
  module type S = S
  module type S2 = S2

  module Make (X : Minimal) : S with module F := X.F and type t := X.t
  module Make2 (X : Minimal2) : S2 with module F := X.F and type 'a t := 'a X.t
end
