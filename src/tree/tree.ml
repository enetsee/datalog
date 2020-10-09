open Core_kernel
open Lib

module Minimal = struct
  type 'a t =
    { lbl : 'a
    ; children : 'a t list
    }
  [@@deriving compare, eq, map, fold, sexp]

  let map t ~f = map f t
  let foldLeft t ~f ~init = fold f init t
  let label { lbl; _ } = lbl
  let children { children; _ } = children
  let branch lbl children = { lbl; children }
  let leaf lbl = branch lbl []

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
    let rec aux { lbl; children } k =
      match children with
      | [] -> k [ [ lbl ] ]
      | _ ->
        mapk aux children
        @@ fun x1 ->
        concatk x1 @@ fun x2 -> mapk (fun xs k -> k (lbl :: xs)) x2 k
    in
    aux t (fun x -> x)
  ;;

  let leaves t =
    let rec aux accu = function
      | { lbl; children = [] } -> lbl :: accu
      | { children; _ } -> List.fold_left ~init:accu ~f:aux children
    in
    aux [] t
  ;;

  let is_leaf { children; _ } = List.is_empty children

  let unfold ~f b =
    let rec aux ~k b =
      let lbl, bs = f b in
      aux_forest bs ~k:(fun children -> k { lbl; children })
    and aux_forest ~k = function
      | [] -> k []
      | next :: rest ->
        aux_forest rest ~k:(fun rest' ->
            aux next ~k:(fun next' -> k @@ (next' :: rest')))
    in
    aux ~k:Fn.id b
  ;;

  module Make (M : Monad.S) = struct
    (** Monadic tree unfold *)
    let unfoldM ~f b =
      let rec aux ~k b =
        M.(
          f b
          >>= fun (a, bs) ->
          aux_forest bs ~k:(fun xs -> k @@ map ~f:(branch a) xs))
      and aux_forest ~k xs =
        M.(
          match xs with
          | [] -> k @@ return []
          | next :: rest ->
            aux_forest rest ~k:(fun rest' ->
                aux next ~k:(fun next' ->
                    next' >>= fun x -> rest' >>= fun xs -> return (x :: xs) |> k)))
      in
      aux ~k:Fn.id b
    ;;
  end

  (** -- Pretty print ------------------------------------------------------- *)

  let with_pos xs =
    let rec aux accu = function
      | [ x ] -> List.rev ((true, x) :: accu)
      | x :: xs -> aux ((false, x) :: accu) xs
      | [] -> List.rev accu
    in
    aux [] xs
  ;;

  let rec pp_root indent pp_a ppf { lbl; children } =
    Fmt.pf ppf {|%a%a|} pp_a lbl (pp_children indent pp_a) children

  and pp_children indent pp_a ppf ts =
    Fmt.(list (pp_child indent pp_a)) ppf (with_pos ts)

  and pp_child indent pp_a ppf (is_last, t) =
    let line = if is_last then "└── " else "├── "
    and indent' = indent ^ if is_last then "    " else "│   " in
    Fmt.pf ppf {|@.%s%s%a|} indent line (pp_root indent' pp_a) t
  ;;

  let pp pp_a ppf x = pp_root "" pp_a ppf x
  let pp = `NoPrec pp
end

include Minimal
include Functor.Make1 (Minimal)
include Foldable.Make1 (Minimal)
include Pretty.Make1 (Minimal)

module Traversable = struct
  module Make (M : sig
    include Monad.S
    include Applicative.S with type 'a t := 'a t
  end) =
  struct
    let rec traverse { lbl; children } ~f =
      M.map2
        ~f:(fun lbl children -> { lbl; children })
        (f lbl)
        (M.all @@ List.map ~f:(traverse ~f) children)
    ;;
  end
end

module Zipper = struct
  type 'a t =
    { node : 'a Minimal.t
    ; path : 'a t list
    ; left : 'a Minimal.t list
    ; right : 'a Minimal.t list
    }
  [@@deriving eq, compare]

  let node { node; _ } = node
  let label { node; _ } = Minimal.label node
  let children { node; _ } = Minimal.children node
  let left { left; _ } = left
  let right { right; _ } = right
  let ancestors { path; _ } = path
  let generation { left; node; right; _ } = List.rev left @ (node :: right)

  let next { node; path; left; right } =
    match right with
    | next :: rest ->
      Some { node = next; path; left = node :: left; right = rest }
    | _ -> None
  ;;

  let prev { node; path; left; right } =
    match left with
    | next :: rest ->
      Some { node = next; path; left = rest; right = node :: right }
    | _ -> None
  ;;

  let first ({ left; right; path; _ } as t) =
    match left with
    | next :: rest ->
      { node = next; left = []; right = List.rev rest @ right; path }
    | _ -> t
  ;;

  let last ({ left; right; path; _ } as t) =
    match List.rev right with
    | next :: rest -> { node = next; left = rest @ left; right = []; path }
    | _ -> t
  ;;

  let up ({ path; _ } as t) =
    match path with
    | next :: rest ->
      Some
        { node = Minimal.branch (label next) (generation t)
        ; left = left next
        ; right = right next
        ; path = rest
        }
    | _ -> None
  ;;

  let rec root t =
    match up t with
    | Some parent -> root parent
    | _ -> t
  ;;

  let first_child t =
    match children t with
    | next :: rest ->
      Some
        { node = next
        ; left = []
        ; right = rest
        ; path = List.cons t @@ ancestors t
        }
    | _ -> None
  ;;

  let down = first_child
  let last_child t = Option.map ~f:last @@ first_child t

  let sibling ({ path; _ } as t) ~n =
    match up t with
    | Some p ->
      let cs = children p in
      (match List.nth cs n with
      | Some c ->
        let left = List.(rev @@ take cs n)
        and right = List.drop cs (n + 1) in
        Some { node = c; left; right; path }
      | _ -> None)
    | _ -> None
  ;;

  let child t ~n = Option.bind ~f:(sibling ~n) @@ first_child t
  let to_tree t = node @@ root t
  let from_tree t = { node = t; left = []; right = []; path = [] }

  let find horiz vert t ~p =
    let rec aux t =
      if p @@ label t
      then Some t
      else Option.(bind ~f:aux @@ first_some (horiz t) (vert t))
    in
    aux t
  ;;

  let find_down t ~p = find next down t ~p
  let find_up t ~p = find prev up t ~p
  let find_root t ~p = find_down ~p @@ root t
end
