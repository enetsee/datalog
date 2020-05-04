include Functor_intf

(*** -- Conversion functors -- ***)

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

(*** -- Make functors --- ***)

module MakeCustom3 (X : Custom3) :
  S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t = struct
  let map = X.map

  let replace =
    match X.replace with
    | `Custom f -> f
    | _ -> fun t ~const -> map ~f:(fun _ -> const) t
  ;;

  module FunctorInfix = struct
    let ( <$> ) f x = map x ~f
    let ( <&> ) x f = map x ~f
    let ( <$ ) x t = map t ~f:(fun _ -> x)
    let ( $> ) t x = map t ~f:(fun _ -> x)
  end

  include FunctorInfix

  let void t = () <$ t
end

module MakeCustom2 (X : Custom2) : S2 with type ('a, 'b) t := ('a, 'b) X.t =
MakeCustom3 (struct
  type ('a, 'b, _) t = ('a, 'b) X.t

  include (X : Custom2 with type ('a, 'b) t := ('a, 'b) X.t)
end)

module MakeCustom1 (X : Custom1) : S1 with type 'a t := 'a X.t =
MakeCustom2 (struct
  type ('a, _) t = 'a X.t

  include (X : Custom1 with type 'a t := 'a X.t)
end)

module Make3 (X : Minimal3) : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t =
MakeCustom3 (struct
  include X

  let replace = `Derived
end)

module Make2 (X : Minimal2) : S2 with type ('a, 'b) t := ('a, 'b) X.t =
Make3 (struct
  type ('a, 'b, _) t = ('a, 'b) X.t

  include (X : Minimal2 with type ('a, 'b) t := ('a, 'b) X.t)
end)

module Make1 (X : Minimal1) : S1 with type 'a t := 'a X.t = Make2 (struct
  type ('a, _) t = 'a X.t

  include (X : Minimal1 with type 'a t := 'a X.t)
end)
