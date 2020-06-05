open Core_kernel

type t =
  | Logical
  | Extralogical of Eff.t list
[@@deriving eq, compare, hash, sexp]

let effects_of = function
  | Extralogical effs -> Eff.Set.of_list effs
  | _ -> Eff.Set.empty
;;

let logical = Logical
let extralogical eff = Extralogical eff
let is_pure t = Eff.Set.is_empty @@ effects_of t
