open Core_kernel
open Lib

module Mode = struct
  (** A `Mode.t` is a requirement on an argument to a predicate. *)
  type t =
    | Opt
    | Req
  [@@deriving compare, sexp]

  (* -- Partial order  ---------------------------------------------------------*)

  let leq t1 t2 =
    match t1, t2 with
    | Opt, _ | Req, Req -> true
    | _ -> false
  ;;

  let max t1 t2 = if leq t1 t2 then t2 else t1
  let min t1 t2 = if leq t1 t2 then t1 else t2

  (* -- Pretty printing  -------------------------------------------------------*)

  let pp ppf = function
    | Req -> Fmt.char ppf '+'
    | Opt -> Fmt.char ppf '?'
  ;;

  let pp_silent ppf = function
    | Req -> Fmt.char ppf '+'
    | _ -> ()
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)
end

module Vector = struct
  exception MismatchArity

  module X = struct
    (** A `ModeVector` indicate the mode requirement of a _predicate_ i.e. the 
      modes requirements of all of its arguments.

    A given predicate may have a set of `ModeVector`s arising either from 
    multiple implementations (in the case of extralogical predicates) or 
    different orderings of subgoals for logical predicates. 
 *)
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
end

include Mode
