(*** -- Minimal definitions using `foldLeft` --- ***)

module type Minimal1 = sig
  include Tycon.S1

  val foldLeft : 'a t -> f:('b -> 'a -> 'b) -> init:'b -> 'b
end

module type Minimal2 = sig
  include Tycon.S2

  val foldLeft : ('a, 'e) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b
end

module type Minimal3 = sig
  include Tycon.S3

  val foldLeft : ('a, 'd, 'e) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b
end

(*** -- Minimal definitions using `foldRight` --- ***)

module type Minimal1_foldRight = sig
  include Tycon.S1

  val foldRight : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b
end

module type Minimal2_foldRight = sig
  include Tycon.S2

  val foldRight : ('a, 'e) t -> f:('a -> 'b -> 'b) -> init:'b -> 'b
end

module type Minimal3_foldRight = sig
  include Tycon.S3

  val foldRight : ('a, 'd, 'e) t -> f:('a -> 'b -> 'b) -> init:'b -> 'b
end

(*** -- Custom definition via `foldLeft` -- ***)

module type Custom1 = sig
  include Minimal1

  val foldRight
    : [ `Derived | `Custom of 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b ]

  val foldMap
    : [ `Derived
      | `Custom of
        (module Monoid.S0 with type t = 'b)
        -> ?init:'b
        -> 'a t
        -> f:('a -> 'b)
        -> 'b
      ]
end

module type Custom2 = sig
  include Minimal2

  val foldRight
    : [ `Derived | `Custom of ('a, _) t -> f:('a -> 'b -> 'b) -> init:'b -> 'b ]

  val foldMap
    : [ `Derived
      | `Custom of
        (module Monoid.S0 with type t = 'b)
        -> ?init:'b
        -> ('a, 'e) t
        -> f:('a -> 'b)
        -> 'b
      ]
end

module type Custom3 = sig
  include Minimal3

  val foldRight
    : [ `Derived
      | `Custom of ('a, _, _) t -> f:('a -> 'b -> 'b) -> init:'b -> 'b
      ]

  val foldMap
    : [ `Derived
      | `Custom of
        (module Monoid.S0 with type t = 'b)
        -> ?init:'b
        -> ('a, 'd, 'e) t
        -> f:('a -> 'b)
        -> 'b
      ]
end

(*** -- Custom definition via `foldRight` -- ***)

module type Custom1_foldRight = sig
  include Minimal1_foldRight

  val foldLeft
    : [ `Derived | `Custom of 'a t -> f:('b -> 'a -> 'b) -> init:'b -> 'b ]

  val foldMap
    : [ `Derived
      | `Custom of
        (module Monoid.S0 with type t = 'b)
        -> ?init:'b
        -> 'a t
        -> f:('a -> 'b)
        -> 'b
      ]
end

module type Custom2_foldRight = sig
  include Minimal2_foldRight

  val foldLeft
    : [ `Derived | `Custom of ('a, _) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b ]

  val foldMap
    : [ `Derived
      | `Custom of
        (module Monoid.S0 with type t = 'b)
        -> ?init:'b
        -> ('a, 'e) t
        -> f:('a -> 'b)
        -> 'b
      ]
end

module type Custom3_foldRight = sig
  include Minimal3_foldRight

  val foldLeft
    : [ `Derived
      | `Custom of ('a, _, _) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b
      ]

  val foldMap
    : [ `Derived
      | `Custom of
        (module Monoid.S0 with type t = 'b)
        -> ?init:'b
        -> ('a, 'd, 'e) t
        -> f:('a -> 'b)
        -> 'b
      ]
end

(*** -- Complete signatures -- ***)

module type S1 = sig
  include Tycon.S1

  val foldLeft : 'a t -> f:('b -> 'a -> 'b) -> init:'b -> 'b
  val foldRight : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b

  val foldMap
    :  (module Monoid.S0 with type t = 'b)
    -> ?init:'b
    -> 'a t
    -> f:('a -> 'b)
    -> 'b

  val fold : (module Monoid.S0 with type t = 'a) -> 'a t -> 'a
  val find : 'a t -> pred:('a -> bool) -> 'a option
  val isEmpty : 'a t -> bool
  val exists : ?init:bool -> 'a t -> pred:('a -> bool) -> bool
  val forAll : ?init:bool -> 'a t -> pred:('a -> bool) -> bool
end

module type S2 = sig
  include Tycon.S2

  val foldLeft : ('a, 'e) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b
  val foldRight : ('a, 'e) t -> f:('a -> 'b -> 'b) -> init:'b -> 'b

  val foldMap
    :  (module Monoid.S0 with type t = 'b)
    -> ?init:'b
    -> ('a, 'e) t
    -> f:('a -> 'b)
    -> 'b

  val fold : (module Monoid.S0 with type t = 'a) -> ('a, 'e) t -> 'a
  val find : ('a, 'e) t -> pred:('a -> bool) -> 'a option
  val isEmpty : ('a, 'e) t -> bool
  val exists : ?init:bool -> ('a, 'e) t -> pred:('a -> bool) -> bool
  val forAll : ?init:bool -> ('a, 'e) t -> pred:('a -> bool) -> bool
end

module type S3 = sig
  include Tycon.S3

  val foldLeft : ('a, 'd, 'e) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b
  val foldRight : ('a, 'd, 'e) t -> f:('a -> 'b -> 'b) -> init:'b -> 'b

  val foldMap
    :  (module Monoid.S0 with type t = 'b)
    -> ?init:'b
    -> ('a, 'd, 'e) t
    -> f:('a -> 'b)
    -> 'b

  val fold : (module Monoid.S0 with type t = 'a) -> ('a, 'd, 'e) t -> 'a
  val find : ('a, 'd, 'e) t -> pred:('a -> bool) -> 'a option
  val isEmpty : ('a, 'd, 'e) t -> bool
  val exists : ?init:bool -> ('a, 'd, 'e) t -> pred:('a -> bool) -> bool
  val forAll : ?init:bool -> ('a, 'd, 'e) t -> pred:('a -> bool) -> bool
end

(*** -- Module signature -- ***)

module type Foldable = sig
  (*** -- Minimal definitions using `foldLeft` --- ***)
  module type Minimal1 = Minimal1
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3

  (*** -- Minimal definitions using `foldRight` --- ***)
  module type Minimal1_foldRight = Minimal1_foldRight
  module type Minimal2_foldRight = Minimal2_foldRight
  module type Minimal3_foldRight = Minimal3_foldRight

  (*** -- Minimal definitions using `foldLeft` --- ***)
  module type Custom1 = Custom1
  module type Custom2 = Custom2
  module type Custom3 = Custom3

  (*** -- Minimal definitions using `foldRight` --- ***)
  module type Custom1_foldRight = Custom1_foldRight
  module type Custom2_foldRight = Custom2_foldRight
  module type Custom3_foldRight = Custom3_foldRight

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

  module MakeCustom1_foldRight (X : Custom1_foldRight) :
    S1 with type 'a t := 'a X.t

  module MakeCustom2_foldRight (X : Custom2_foldRight) :
    S2 with type ('a, 'b) t := ('a, 'b) X.t

  module MakeCustom3_foldRight (X : Custom3_foldRight) :
    S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t

  module Make1 (X : Minimal1) : S1 with type 'a t := 'a X.t
  module Make2 (X : Minimal2) : S2 with type ('a, 'b) t := ('a, 'b) X.t
  module Make3 (X : Minimal3) : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t
  module Make1_foldRight (X : Minimal1_foldRight) : S1 with type 'a t := 'a X.t

  module Make2_foldRight (X : Minimal2_foldRight) :
    S2 with type ('a, 'b) t := ('a, 'b) X.t

  module Make3_foldRight (X : Minimal3_foldRight) :
    S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t
end
