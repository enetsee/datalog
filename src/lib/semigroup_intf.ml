(*** -- Minimal definitions -- ***)

module type Minimal0 = sig
  type t

  val append : t -> t -> t
end

module type Minimal1 = sig
  type 'a t

  val append : 'a t -> 'a t -> 'a t
end

module type Minimal2 = sig
  type ('a, 'b) t

  val append : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
end

module type Minimal3 = sig
  type ('a, 'b, 'c) t

  val append : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
end

(*** -- Infix functions -- ***)

module type Infix0 = sig
  type t

  val ( <> ) : t -> t -> t
end

module type Infix1 = sig
  type 'a t

  val ( <> ) : 'a t -> 'a t -> 'a t
end

module type Infix2 = sig
  type ('a, 'b) t

  val ( <> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
end

module type Infix3 = sig
  type ('a, 'b, 'c) t

  val ( <> ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
end

(*** -- Complete definitions -- ***)

module type S0 = sig
  include Minimal0
  module SemigroupInfix : Infix0 with type t := t
  include Infix0 with type t := t
end

module type S1 = sig
  include Minimal1
  module SemigroupInfix : Infix1 with type 'a t := 'a t
  include Infix1 with type 'a t := 'a t
end

module type S2 = sig
  include Minimal2
  module SemigroupInfix : Infix2 with type ('a, 'b) t := ('a, 'b) t
  include Infix2 with type ('a, 'b) t := ('a, 'b) t
end

module type S3 = sig
  include Minimal3
  module SemigroupInfix : Infix3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  include Infix3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end

(*** -- Module signature -- ***)

module type Semigroup = sig
  (*** -- Minimal definitions --- ***)
  module type Minimal0 = Minimal0
  module type Minimal1 = Minimal1
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3

  (*** -- Infix functions --- ***)
  module type Infix0 = Infix0
  module type Infix1 = Infix1
  module type Infix2 = Infix2
  module type Infix3 = Infix3

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
end
