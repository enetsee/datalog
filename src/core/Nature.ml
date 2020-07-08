open Core_kernel
open Lib

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

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
    | Logical -> Fmt.string ppf "Logical"
    | Extralogical eff ->
      Fmt.(hovbox @@ pair ~sep:sp string @@ braces @@ list ~sep:comma Eff.pp)
        ppf
        ("Extralogical", eff)
  ;;

  let pp = `NoPrec pp
end)
