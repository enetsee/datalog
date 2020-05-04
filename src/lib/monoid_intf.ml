(*** -- Minimal definitions -- ***)

module type Minimal0 = sig
  include Semigroup.Minimal0

  val mempty : t
end

module type Minimal1 = sig
  include Semigroup.Minimal1

  val mempty : 'a t
end

module type Minimal2 = sig
  include Semigroup.Minimal2

  val mempty : ('a, 'b) t
end

module type Minimal3 = sig
  include Semigroup.Minimal3

  val mempty : ('a, 'b, 'c) t
end

(*** -- Complete definitions -- ***)

module type S0 = sig
  include Minimal0
  include Semigroup.S0 with type t := t
end

module type S1 = sig
  include Minimal1
  include Semigroup.S1 with type 'a t := 'a t
end

module type S2 = sig
  include Minimal2
  include Semigroup.S2 with type ('a, 'b) t := ('a, 'b) t
end

module type S3 = sig
  include Minimal3
  include Semigroup.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end

module type Monoid = sig
  (*** -- Minimal definitions --- ***)
  module type Minimal0 = Minimal0
  module type Minimal1 = Minimal1
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3

  (*** -- Complete definitions -- ***)
  module type S0 = S0
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3

  (*** -- Functors -- ***)
  module S0_to_S1 (X : S0) : S1 with type 'a t = X.t
  module S1_to_S0 (X : S1) : S0 with type t = unit X.t
  module S1_to_S2 (X : S1) : S2 with type ('a, _) t = 'a X.t
  module S2_to_S1 (X : S2) : S1 with type 'a t = ('a, unit) X.t
  module S2_to_S3 (X : S2) : S3 with type ('a, 'b, _) t = ('a, 'b) X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b, unit) X.t
  module Make0 (X : Minimal0) : S0 with type t := X.t
  module Make1 (X : Minimal1) : S1 with type 'a t := 'a X.t
  module Make2 (X : Minimal2) : S2 with type ('a, 'b) t := ('a, 'b) X.t
  module Make3 (X : Minimal3) : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t
  module MakeDual0 (X : Minimal0) : S0 with type t := X.t
  module MakeDual1 (X : Minimal1) : S1 with type 'a t := 'a X.t
  module MakeDual2 (X : Minimal2) : S2 with type ('a, 'b) t := ('a, 'b) X.t

  module MakeDual3 (X : Minimal3) :
    S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t

  (*** -- Instances -- ***)
  module Or : S0 with type t = bool
  module And : S0 with type t = bool
end
