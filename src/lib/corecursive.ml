include Corecursive_intf

module Make (X : Minimal) : S with module F := X.F and type t := X.t = struct
  type 'a coalgebra = 'a -> 'a X.F.t

  let rec ana f x = f x |> X.F.map ~f:(ana f) |> X.embed
  let transform_top_down f x = ana (fun x -> X.proj @@ f x) x

  type 'a r_coalgebra = 'a -> (X.t, 'a) result X.F.t

  let result r ~ok ~err =
    match r with
    | Ok x -> ok x
    | Error x -> err x
  ;;

  let rec apo f x =
    f x |> X.F.map ~f:(result ~ok:(fun x -> x) ~err:(apo f)) |> X.embed
  ;;

  let transform_partial f =
    let align x = X.F.map ~f:(fun x -> Ok x) @@ X.proj x in
    let sequence = result ~ok:align ~err:align in
    apo (fun x -> sequence @@ f x)
  ;;
end

module Make2 (X : Minimal2) : S2 with module F := X.F and type 'a t := 'a X.t =
struct
  type ('a, 'e) coalgebra = 'a -> ('a, 'e) X.F.t

  let rec ana f x = f x |> X.F.map ~f:(ana f) |> X.embed
  let transform_top_down f x = ana (fun x -> X.proj @@ f x) x

  type ('a, 'e) r_coalgebra = 'a -> (('e X.t, 'a) result, 'e) X.F.t

  let result r ~ok ~err =
    match r with
    | Ok x -> ok x
    | Error x -> err x
  ;;

  let rec apo f x =
    f x |> X.F.map ~f:(result ~ok:(fun x -> x) ~err:(apo f)) |> X.embed
  ;;

  let transform_partial f =
    let align x = X.F.map ~f:(fun x -> Ok x) @@ X.proj x in
    let sequence = result ~ok:align ~err:align in
    apo (fun x -> sequence @@ f x)
  ;;
end
