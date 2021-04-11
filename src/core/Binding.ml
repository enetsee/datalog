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
