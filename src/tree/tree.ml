open Core_kernel
open Lib

type 'a t = Node of 'a * 'a t list [@@deriving compare, map, fold, sexp]

let branch lbl forest = Node (lbl, forest)
let leaf lbl = Node (lbl, [])

let rec flatten (Node (x, ts)) : 'a list list =
  match ts with
  | [] -> [ [ x ] ]
  | ts -> List.map ~f:(fun xs -> x :: xs) @@ List.concat_map ~f:flatten ts
;;

let subpaths t =
  let rec aux ~k (Node (x, ts)) =
    aux_forest ts ~k:(fun xss -> k @@ List.map ~f:(fun xs -> x :: xs) xss)
  and aux_forest ~k = function
    | [] -> k [ [] ]
    | next :: rest ->
      aux next ~k:(fun xs -> aux_forest rest ~k:(fun xss -> k @@ xs @ xss))
  in
  aux ~k:Fn.id t
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
