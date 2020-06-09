open Core_kernel
open Lib

type 'a t = Node of 'a * 'a t list [@@deriving compare, eq, map, fold, sexp]

let label (Node (lbl, _)) = lbl
let children (Node (_, cs)) = cs
let branch lbl forest = Node (lbl, forest)
let leaf lbl = Node (lbl, [])

let flatten t =
  let rec mapk f l k =
    match l with
    | [] -> k []
    | x :: xs -> f x @@ fun y -> mapk f xs @@ fun ys -> k (y :: ys)
  in
  let rec concatk l k =
    match l with
    | [] -> k []
    | l :: r -> concatk r @@ fun r' -> k (l @ r')
  in
  let rec aux (Node (x, ts)) k =
    match ts with
    | [] -> k [ [ x ] ]
    | ts ->
      mapk aux ts
      @@ fun x1 -> concatk x1 @@ fun x2 -> mapk (fun xs k -> k (x :: xs)) x2 k
  in
  aux t (fun x -> x)
;;

let leaves t =
  let rec aux accu = function
    | Node (t, []) -> t :: accu
    | Node (_, xs) -> List.fold_left ~init:accu ~f:aux xs
  in
  aux [] t
;;

let is_leaf (Node (_, ts)) =
  match ts with
  | [] -> true
  | _ -> false
;;

let with_pos xs =
  let rec aux accu = function
    | [ x ] -> List.rev ((true, x) :: accu)
    | x :: xs -> aux ((false, x) :: accu) xs
    | [] -> List.rev accu
  in
  aux [] xs
;;

(* -- Functor implementation ------------------------------------------------ *)

include Functor.Make1 (struct
  type nonrec 'a t = 'a t

  let map t ~f = map f t
end)

(* -- Foldable implementation ----------------------------------------------- *)

include Foldable.Make1 (struct
  type nonrec 'a t = 'a t

  let foldLeft t ~f ~init = fold f init t
end)

(* -- Unfold ---------------------------------------------------------------- *)

let unfold ~f b =
  let rec aux ~k b =
    let a, bs = f b in
    aux_forest bs ~k:(fun xs -> k @@ Node (a, xs))
  and aux_forest ~k = function
    | [] -> k []
    | next :: rest ->
      aux_forest rest ~k:(fun rest' ->
          aux next ~k:(fun next' -> k @@ (next' :: rest')))
  in
  aux ~k:Fn.id b
;;

(* -- Pretty implementation ------------------------------------------------- *)

include Pretty.Make1 (struct
  type nonrec 'a t = 'a t

  let rec pp_root indent pp_a ppf (Node (lbl, ts)) =
    Fmt.pf ppf {|%a%a|} pp_a lbl (pp_children indent pp_a) ts

  and pp_children indent pp_a ppf ts =
    Fmt.(list (pp_child indent pp_a)) ppf (with_pos ts)

  and pp_child indent pp_a ppf (is_last, t) =
    let line = if is_last then "└── " else "├── "
    and indent' = indent ^ if is_last then "    " else "│   " in
    Fmt.pf ppf {|@.%s%s%a|} indent line (pp_root indent' pp_a) t
  ;;

  let pp pp_a ppf x = pp_root "" pp_a ppf x
  let pp = `NoPrec pp
end)
