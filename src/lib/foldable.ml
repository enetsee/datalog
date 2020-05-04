include Foldable_intf

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

module type Complete3 = sig
  include Tycon.S3

  val foldLeft : ('a, 'd, 'e) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b
  val foldRight : ('a, 'd, 'e) t -> f:('a -> 'b -> 'b) -> init:'b -> 'b

  val foldMap
    :  (module Monoid.S0 with type t = 'b)
    -> ?init:'b
    -> ('a, 'd, 'e) t
    -> f:('a -> 'b)
    -> 'b
end

module CompleteCustom3_foldLeft (X : Custom3) :
  Complete3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t = struct
  let foldLeft = X.foldLeft

  let foldRight_ x ~f ~init =
    let f' k x z = k @@ f x z in
    foldLeft ~f:f' ~init:(fun x -> x) x init
  ;;

  let foldRight =
    match X.foldRight with
    | `Custom f -> f
    | _ -> foldRight_
  ;;

  let foldMap_
      (type a)
      (module M : Monoid.S0 with type t = a)
      ?init:(empty = M.mempty)
      x
      ~f
    =
    foldRight ~f:(fun x accu -> M.append accu @@ f x) ~init:empty x
  ;;

  let foldMap =
    match X.foldMap with
    | `Custom f -> f
    | _ -> foldMap_
  ;;
end

module CompleteCustom3_foldRight (X : Custom3_foldRight) :
  Complete3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t = struct
  let foldRight = X.foldRight

  let foldLeft_ x ~f ~init =
    let f' x k z = k @@ f z x in
    foldRight ~f:f' ~init:(fun x -> x) x init
  ;;

  let foldLeft =
    match X.foldLeft with
    | `Custom f -> f
    | _ -> foldLeft_
  ;;

  let foldMap_
      (type a)
      (module M : Monoid.S0 with type t = a)
      ?init:(empty = M.mempty)
      x
      ~f
    =
    foldRight ~f:(fun x accu -> M.append accu @@ f x) ~init:empty x
  ;;

  let foldMap =
    match X.foldMap with
    | `Custom f -> f
    | _ -> foldMap_
  ;;
end

module MakeComplete3 (X : Complete3) :
  S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t = struct
  include X

  let exists ?init x ~pred = foldMap (module Monoid.Or) x ~f:pred ?init
  let forAll ?init x ~pred = foldMap (module Monoid.And) x ~f:pred ?init

  let find x ~pred =
    foldRight
      ~f:(fun x accu ->
        match accu with
        | Some _ -> accu
        | _ -> if pred x then Some x else None)
      ~init:None
      x
  ;;

  let fold (type a) (module M : Monoid.S0 with type t = a) t =
    foldMap (module M) ~f:(fun x -> x) t
  ;;

  let isEmpty t = foldRight ~f:(fun _ _ -> false) ~init:true t
end

module MakeCustom3 (X : Custom3) :
  S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t = MakeComplete3 (struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) X.t

  include CompleteCustom3_foldLeft (X)
end)

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

module MakeCustom3_foldRight (X : Custom3_foldRight) :
  S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t = MakeComplete3 (struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) X.t

  include CompleteCustom3_foldRight (X)
end)

module MakeCustom2_foldRight (X : Custom2_foldRight) :
  S2 with type ('a, 'b) t := ('a, 'b) X.t = MakeCustom3_foldRight (struct
  type ('a, 'b, _) t = ('a, 'b) X.t

  include (X : Custom2_foldRight with type ('a, 'b) t := ('a, 'b) X.t)
end)

module MakeCustom1_foldRight (X : Custom1_foldRight) :
  S1 with type 'a t := 'a X.t = MakeCustom2_foldRight (struct
  type ('a, _) t = 'a X.t

  include (X : Custom1_foldRight with type 'a t := 'a X.t)
end)

module Make3 (X : Minimal3) : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t =
MakeCustom3 (struct
  include X

  let foldRight = `Derived
  let foldMap = `Derived
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

module Make3_foldRight (X : Minimal3_foldRight) :
  S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t =
MakeCustom3_foldRight (struct
  include X

  let foldLeft = `Derived
  let foldMap = `Derived
end)

module Make2_foldRight (X : Minimal2_foldRight) :
  S2 with type ('a, 'b) t := ('a, 'b) X.t = Make3_foldRight (struct
  type ('a, 'b, _) t = ('a, 'b) X.t

  include (X : Minimal2_foldRight with type ('a, 'b) t := ('a, 'b) X.t)
end)

module Make1_foldRight (X : Minimal1_foldRight) : S1 with type 'a t := 'a X.t =
Make2_foldRight (struct
  type ('a, _) t = 'a X.t

  include (X : Minimal1_foldRight with type 'a t := 'a X.t)
end)
