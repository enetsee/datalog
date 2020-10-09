open Core_kernel
open Lib

(** A variable binding indicates whether a constant value must be provided
    when evaluating a predicate *)
type var =
  | Free
  | Bound
[@@deriving eq, compare, sexp]

(** A `Binding` pattern is a list of variable bindings equal in length 
    to the arity of the predicate *)
type t = { pattern : var list } [@@deriving compare, eq, sexp]

(* let var_well_moded t ~mode =
  match t, mode with
  | _, Mode.Opt | Bound, Req -> true
  | Free, Req -> false
;; *)

let pp_var ppf = function
  | Free -> Fmt.char ppf 'f'
  | Bound -> Fmt.char ppf 'b'
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { pattern } = Fmt.(hbox @@ list pp_var) ppf pattern
  let pp = `NoPrec pp
end)

let mk_free pred =
  { pattern = List.init ~f:Fn.(const Free) @@ Pred.arity_of pred }
;;

let mk_bound pred =
  { pattern = List.init ~f:Fn.(const Bound) @@ Pred.arity_of pred }
;;

let to_list { pattern } = pattern
let from_list pattern = { pattern }

(* let to_atomic_constraint { pattern } =
  Constraint.Atomic.of_list
  @@ List.filter ~f:(fun x -> x > -1)
  @@ List.mapi pattern ~f:(fun idx ->
       function
       | Bound -> idx
       | _ -> -1)
;;

let consistent t ~atom =
  Constraint.Atomic.is_subset atom ~of_:(to_atomic_constraint t)
;;

let well_moded t ~cnstr =
  let bp = to_atomic_constraint t in
  List.exists ~f:(fun atom -> Constraint.Atomic.is_subset atom ~of_:bp)
  @@ Constraint.elements cnstr
;; *)
