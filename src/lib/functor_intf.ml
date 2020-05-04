(*** -- Minimal definitions --- ***)

module type Minimal1 = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module type Minimal2 = sig
  type ('a, 'b) t

  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
end

module type Minimal3 = sig
  type ('a, 'b, 'c) t

  val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
end

(*** -- Custom definitions -- ***)

module type Custom1 = sig
  include Minimal1

  val replace : [ `Derived | `Custom of 'a t -> const:'b -> 'b t ]
end

module type Custom2 = sig
  include Minimal2

  val replace : [ `Derived | `Custom of ('a, 'e) t -> const:'b -> ('b, 'e) t ]
end

module type Custom3 = sig
  include Minimal3

  val replace
    : [ `Derived | `Custom of ('a, 'd, 'e) t -> const:'b -> ('b, 'd, 'e) t ]
end

(*** -- Infix functions -- ***)

module type Infix = sig
  type 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <$ ) : 'a -> 'b t -> 'a t
  val ( $> ) : 'b t -> 'a -> 'a t
end

module type Infix2 = sig
  type ('a, 'e) t

  val ( <$> ) : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
  val ( <&> ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  val ( <$ ) : 'b -> ('a, 'e) t -> ('b, 'e) t
  val ( $> ) : ('a, 'e) t -> 'b -> ('b, 'e) t
end

module type Infix3 = sig
  type ('a, 'd, 'e) t

  val ( <$> ) : ('a -> 'b) -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
  val ( <&> ) : ('a, 'd, 'e) t -> ('a -> 'b) -> ('b, 'd, 'e) t
  val ( <$ ) : 'b -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
  val ( $> ) : ('a, 'd, 'e) t -> 'b -> ('b, 'd, 'e) t
end

(*** -- Complete signatures -- ***)

module type S1 = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val replace : 'a t -> const:'b -> 'b t
  val void : 'a t -> unit t

  module FunctorInfix : Infix with type 'a t := 'a t
  include Infix with type 'a t := 'a t
end

module type S2 = sig
  type ('a, 'e) t

  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
  val replace : ('a, 'e) t -> const:'b -> ('b, 'e) t
  val void : ('a, 'e) t -> (unit, 'e) t

  module FunctorInfix : Infix2 with type ('a, 'b) t := ('a, 'b) t
  include Infix2 with type ('a, 'b) t := ('a, 'b) t
end

module type S3 = sig
  type ('a, 'd, 'e) t

  val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
  val replace : ('a, 'd, 'e) t -> const:'b -> ('b, 'd, 'e) t
  val void : ('a, 'd, 'e) t -> (unit, 'd, 'e) t

  module FunctorInfix : Infix3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  include Infix3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end

(*** -- Module signature -- ***)

module type Functor = sig
  (*** -- Minimal definitions --- ***)
  module type Minimal1 = Minimal1
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3

  (*** -- Custom definitions --- ***)
  module type Custom1 = Custom1
  module type Custom2 = Custom2
  module type Custom3 = Custom3

  (*** -- Infix functions --- ***)
  module type Infix = Infix
  module type Infix2 = Infix2
  module type Infix3 = Infix3

  (*** -- Complete definitions -- ***)
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3

  (*** -- Functors -- ***)
  module S1_to_S2 (X : S1) : S2 with type ('a, _) t = 'a X.t
  module S2_to_S1 (X : S2) : S1 with type 'a t = ('a, unit) X.t
  module S2_to_S3 (X : S2) : S3 with type ('a, 'b, _) t = ('a, 'b) X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b, unit) X.t
  module MakeCustom1 (X : Custom1) : S1 with type 'a t := 'a X.t
  module MakeCustom2 (X : Custom2) : S2 with type ('a, 'b) t := ('a, 'b) X.t

  module MakeCustom3 (X : Custom3) :
    S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t

  module Make1 (X : Minimal1) : S1 with type 'a t := 'a X.t
  module Make2 (X : Minimal2) : S2 with type ('a, 'b) t := ('a, 'b) X.t
  module Make3 (X : Minimal3) : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t
end
