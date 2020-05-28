open Core_kernel
open Lib

exception MismatchArity

module X = struct
  type t = { mv : Mode.t list } [@@deriving compare, sexp]

  let to_list { mv } = mv
  let from_list mv = { mv }

  let combine { mv = xs } { mv = ys } =
    match List.map2 ~f:Mode.max xs ys with
    | Ok mv -> { mv }
    | _ -> raise MismatchArity
  ;;

  let pp_ ppf { mv } = Fmt.(hbox @@ list Mode.pp) ppf mv
  let pp = `NoPrec pp_
end

module Set = struct
  include Set.Make (X)

  let combine cs1 cs2 =
    List.(
      elements cs1
      >>= fun c1 -> elements cs2 >>= fun c2 -> return (X.combine c1 c2))
    |> of_list
  ;;
end

include X
include Pretty.Make0 (X)

(* -- Partial order  ---------------------------------------------------------*)

let leq { mv = xs } { mv = ys } =
  let rec aux = function
    | [], [] -> true
    | x :: xs, y :: ys -> if Mode.(leq x y) then aux (xs, ys) else false
    | [], _ | _, [] -> false
  in
  aux (xs, ys)
;;
