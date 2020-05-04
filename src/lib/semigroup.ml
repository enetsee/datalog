include Semigroup_intf

(*** -- Conversion functors -- ***)

module S0_to_S1 (X : S0) : S1 with type 'a t = X.t = struct
  type 'a t = X.t

  include (X : S0 with type t := X.t)
end

module S1_to_S0 (X : S1) : S0 with type t = unit X.t = struct
  type t = unit X.t

  include (X : S1 with type 'a t := 'a X.t)
end

module S1_to_S2 (X : S1) : S2 with type ('a, _) t = 'a X.t = struct
  type ('a, _) t = 'a X.t

  include (X : S1 with type 'a t := 'a X.t)
end

module S2_to_S1 (X : S2) : S1 with type 'a t = ('a, unit) X.t = struct
  type 'a t = ('a, unit) X.t

  include (X : S2 with type ('a, 'b) t := ('a, 'b) X.t)
end

module S2_to_S3 (X : S2) : S3 with type ('a, 'b, _) t = ('a, 'b) X.t = struct
  type ('a, 'b, _) t = ('a, 'b) X.t

  include (X : S2 with type ('a, 'b) t := ('a, 'b) X.t)
end

module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b, unit) X.t = struct
  type ('a, 'b) t = ('a, 'b, unit) X.t

  include (X : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t)
end

module Make3 (X : Minimal3) : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t =
struct
  let append = X.append

  module SemigroupInfix = struct
    let ( <> ) = append
  end

  include SemigroupInfix
end

module Make2 (X : Minimal2) : S2 with type ('a, 'b) t := ('a, 'b) X.t =
Make3 (struct
  type ('a, 'b, _) t = ('a, 'b) X.t

  include (X : Minimal2 with type ('a, 'b) t := ('a, 'b) X.t)
end)

module Make1 (X : Minimal1) : S1 with type 'a t := 'a X.t = Make2 (struct
  type ('a, _) t = 'a X.t

  include (X : Minimal1 with type 'a t := 'a X.t)
end)

module Make0 (X : Minimal0) : S0 with type t := X.t = Make1 (struct
  type 'a t = X.t

  include (X : Minimal0 with type t := X.t)
end)

module MakeDual3 (X : Minimal3) :
  S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t = Make3 (struct
  type nonrec ('a, 'b, 'c) t = ('a, 'b, 'c) X.t

  let append x y = X.append y x
end)

module MakeDual2 (X : Minimal2) : S2 with type ('a, 'b) t := ('a, 'b) X.t =
MakeDual3 (struct
  type ('a, 'b, _) t = ('a, 'b) X.t

  include (X : Minimal2 with type ('a, 'b) t := ('a, 'b) X.t)
end)

module MakeDual1 (X : Minimal1) : S1 with type 'a t := 'a X.t =
MakeDual2 (struct
  type ('a, _) t = 'a X.t

  include (X : Minimal1 with type 'a t := 'a X.t)
end)

module MakeDual0 (X : Minimal0) : S0 with type t := X.t = MakeDual1 (struct
  type 'a t = X.t

  include (X : Minimal0 with type t := X.t)
end)
