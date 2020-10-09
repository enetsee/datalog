open Core_kernel
open Core
open Lib

(** An edge contains:
      - `scheduled` literals from those available in the source vertex; and 
      - `cost` of scheduling those literals *)
type t =
  { scheduled : Lit.Raw.Set.t
  ; cost : Var.Set.t
  }
[@@deriving compare, eq]

let empty = { scheduled = Lit.Raw.Set.empty; cost = Var.Set.empty }

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { scheduled; cost } =
    Fmt.(
      hovbox @@ pair ~sep:comma (braces @@ list ~sep:comma Pred.pp) Var.Set.pp)
      ppf
      (List.map ~f:Lit.Raw.(pred_of) @@ Lit.Raw.Set.elements scheduled, cost)
  ;;

  let pp = `NoPrec pp
end)
