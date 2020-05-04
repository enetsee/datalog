include Recursive_intf

module Make (X : Minimal) : S with module F := X.F and type t := X.t = struct
  type 'a algebra = 'a X.F.t -> 'a

  let rec cata f x = X.proj x |> X.F.map ~f:(cata f) |> f
  let transform_bottom_up f x = cata (fun x -> f @@ X.embed x) x

  type 'a r_algebra = X.t -> 'a X.F.t -> 'a

  let rec para f t = X.proj t |> X.F.map ~f:(para f) |> f t

  let transform_with_context f x =
    para (fun ctxt projected -> f ctxt @@ X.embed projected) x
  ;;
end

module Make2 (X : Minimal2) : S2 with module F := X.F and type 'a t := 'a X.t =
struct
  type ('a, 'e) algebra = ('a, 'e) X.F.t -> 'a

  let rec cata f x = X.proj x |> X.F.map ~f:(cata f) |> f
  let transform_bottom_up f x = cata (fun x -> f @@ X.embed x) x

  type ('a, 'e) r_algebra = 'e X.t -> ('a, 'e) X.F.t -> 'a

  let rec para f t = X.proj t |> X.F.map ~f:(para f) |> f t

  let transform_with_context f x =
    para (fun ctxt projected -> f ctxt @@ X.embed projected) x
  ;;
end
