open Core_kernel
open Lib

type t = { bp : Adornment.t list } [@@deriving compare, sexp]

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { bp } = Fmt.(hbox @@ list Adornment.pp) ppf bp
  let pp = `NoPrec pp
end)

let to_list { bp } = bp
let from_list bp = { bp }

let to_atomic_constraint { bp } =
  Constraint.Atomic.of_list
  @@ List.filter ~f:(fun x -> x > -1)
  @@ List.mapi bp ~f:(fun idx ->
       function
       | Adornment.Bound -> idx
       | _ -> -1)
;;

let consistent t ~atom =
  Constraint.Atomic.is_subset atom ~of_:(to_atomic_constraint t)
;;

let well_moded t ~cnstr =
  let bp = to_atomic_constraint t in
  List.exists ~f:(fun atom -> Constraint.Atomic.is_subset atom ~of_:bp)
  @@ Constraint.elements cnstr
;;
