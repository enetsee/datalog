open Core_kernel
open Core
open Lib

(** A vertex in a schedule graph contains:    
    - `available` literals yet to be scheduled with their original position in 
      the clause and associated obligation
    - `bound` variables of the scheduled literals
    - `debt` i.e. the variables that must be bound in the head of the clause
      to make the ordering well-moded.
    *)
type t =
  { available : (int * Lit.Raw.t * Obligation.t) list
  ; bound : Var.Set.t
  ; debt : Var.Set.t
  }
[@@deriving compare, eq]

let make ?(bound = Var.Set.empty) ?(debt = Var.Set.empty) available =
  { available; bound; debt }
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp_alt_elem ppf (_, lit, _) = Pred.pp ppf @@ Lit.Raw.pred_of lit

  let pp_alt ppf alt =
    Fmt.(hovbox @@ parens @@ list ~sep:comma pp_alt_elem) ppf alt
  ;;

  let pp ppf { available; bound; debt } =
    Fmt.(
      hovbox
      @@ pair ~sep:comma pp_alt
      @@ pair
           ~sep:sp
           (prefix (any "bound: ") Var.Set.pp)
           (prefix (any "debt: ") Var.Set.pp))
      ppf
      (available, (bound, debt))
  ;;

  let pp = `NoPrec pp
end)
