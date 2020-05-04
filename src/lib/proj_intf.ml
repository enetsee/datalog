module type Minimal = sig
  module F : Functor.S1

  type t

  val proj : t -> t F.t
end

module type Minimal2 = sig
  module F : Functor.S2

  type 'a t

  val proj : 'a t -> ('a t, 'a) F.t
end

module type S = sig
  include Minimal

  val proj_succ : (t -> t F.t) -> t -> t F.t F.t
  val proj2 : t -> t F.t F.t
  val proj3 : t -> t F.t F.t F.t
  val proj4 : t -> t F.t F.t F.t F.t
  val proj5 : t -> t F.t F.t F.t F.t F.t
end

module type S2 = sig
  include Minimal2

  val proj_succ : ('a t -> ('a t, 'a) F.t) -> 'a t -> (('a t, 'a) F.t, 'a) F.t
  val proj2 : 'a t -> (('a t, 'a) F.t, 'a) F.t
  val proj3 : 'a t -> ((('a t, 'a) F.t, 'a) F.t, 'a) F.t
  val proj4 : 'a t -> (((('a t, 'a) F.t, 'a) F.t, 'a) F.t, 'a) F.t
  val proj5 : 'a t -> ((((('a t, 'a) F.t, 'a) F.t, 'a) F.t, 'a) F.t, 'a) F.t
end

module type Proj = sig
  module type Minimal = Minimal
  module type Minimal2 = Minimal2
  module type S = S
  module type S2 = S2

  module Make (X : Minimal) : S with type t := X.t and module F := X.F
  module Make2 (X : Minimal2) : S2 with type 'a t := 'a X.t and module F := X.F
end
