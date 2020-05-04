module type Minimal0 = sig
  type t

  val pp : [ `WithPrec of int -> t Fmt.t | `NoPrec of t Fmt.t ]
end

module type S0 = sig
  type t

  val pp : t Fmt.t
  val pp_prec : int -> t Fmt.t
  val to_string : t -> string
end

module type Minimal1 = sig
  type 'a t

  val pp
    : [ `WithPrec of int -> (int -> 'a Fmt.t) -> 'a t Fmt.t
      | `NoPrec of 'a Fmt.t -> 'a t Fmt.t
      ]
end

module type S1 = sig
  type 'a t

  val pp : 'a Fmt.t -> 'a t Fmt.t
  val pp_prec : int -> (int -> 'a Fmt.t) -> 'a t Fmt.t
  val to_string : 'a t -> pp_a:'a Fmt.t -> string
end

module type Minimal2 = sig
  type ('a, 'b) t

  val pp
    : [ `NoPrec of 'a Fmt.t -> 'b Fmt.t -> ('a, 'b) t Fmt.t
      | `WithPrec of
        int -> (int -> 'a Fmt.t) -> (int -> 'b Fmt.t) -> ('a, 'b) t Fmt.t
      ]
end

module type S2 = sig
  type ('a, 'b) t

  val pp : 'a Fmt.t -> 'b Fmt.t -> ('a, 'b) t Fmt.t

  val pp_prec
    :  int
    -> (int -> 'a Fmt.t)
    -> (int -> 'b Fmt.t)
    -> ('a, 'b) t Fmt.t

  val to_string : ('a, 'b) t -> pp_a:'a Fmt.t -> pp_b:'b Fmt.t -> string
end

module type Pretty = sig
  module type Minimal0 = Minimal0
  module type Minimal1 = Minimal1
  module type Minimal2 = Minimal2
  module type S0 = S0
  module type S1 = S1
  module type S2 = S2

  module Make0 (X : Minimal0) : S0 with type t := X.t
  module Make1 (X : Minimal1) : S1 with type 'a t := 'a X.t
  module Make2 (X : Minimal2) : S2 with type ('a, 'b) t := ('a, 'b) X.t
end
