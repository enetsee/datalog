open Core_kernel

module type S1 = sig
  type 'a t

  module Traversable (A : Applicative.S) : sig
    val traverse : 'a t -> f:('a -> 'b A.t) -> 'b t A.t
    val sequence : 'a A.t t -> 'a t A.t
  end

  module Traversable2 (A : Applicative.S2) : sig
    val traverse : 'a t -> f:('a -> ('b, 'e) A.t) -> ('b t, 'e) A.t
    val sequence : ('a, 'e) A.t t -> ('a t, 'e) A.t
  end

  module Traversable3 (A : Applicative.S3) : sig
    val traverse : 'a t -> f:('a -> ('b, 'd, 'e) A.t) -> ('b t, 'd, 'e) A.t
    val sequence : ('a, 'd, 'e) A.t t -> ('a t, 'd, 'e) A.t
  end
end
