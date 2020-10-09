open Core_kernel
open Core
open Lib
include Set.Make (Var.Set)

let trivial = singleton Var.Set.empty
let is_trivial t = equal trivial t

(** 
  The minimum of an obligation is the set of it's atomic constraints
  which are not contained in another atomic constraint.

  Example:

  min_of {{x,y},{x},{y},{z}} = {{x,y},{z}}
*)
let min_of in_ =
  let xs, orig =
    match in_ with
    | `ListIn xs -> xs, of_list xs
    | `SetIn orig -> to_list orig, orig
  in
  of_list
  @@ List.filter
       ~f:(fun x -> not @@ exists ~f:(fun y -> Var.Set.lt y x) orig)
       xs
;;

let is_obligated acstr idx = function
  | _ when not @@ Constraint.Atomic.mem acstr idx -> None
  | Term.TVar (v, _) -> Some (Var.Named v)
  | TWild _ -> Some (Var.Wild idx)
  | _ -> None
;;

(** The obligation of a _constrained_ literal is a translation from its 
    dataflow constraint to the literals variables, excluding constants or 
    parameters.

    Examples:

    `of_constrained_lit p(x,y,z) {{0,2},{0,1},{1,2}} = 
        {{x,z},{x,y},{y,z}}
    `

    `of_constrained_lit p(x,_,A) {{0,2},{0,1},{1,2}} = 
      {{0@x,1@_},{x,1@_},{1@_}}
    `
    
*)
let of_constrained_lit lit ~cstr =
  let terms = Lit.Raw.terms_of lit in
  Constraint.elements cstr
  |> List.map ~f:(fun acstr ->
         let f = is_obligated acstr in
         terms |> List.filter_mapi ~f |> Var.Set.of_list)
  |> of_list
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf t =
    Fmt.(hovbox @@ braces @@ list ~sep:comma Var.Set.pp) ppf @@ elements t
  ;;

  let pp = `NoPrec pp
end)
