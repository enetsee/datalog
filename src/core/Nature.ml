type t =
  | Logical
  | Extralogical of Eff.Set.t
[@@deriving eq, compare, sexp]

let effects_of = function
  | Extralogical effs -> effs
  | _ -> Eff.Set.empty
;;

let logical = Logical
let extralogical eff = Extralogical eff
let is_pure t = Eff.Set.is_empty @@ effects_of t
