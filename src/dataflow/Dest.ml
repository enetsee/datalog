open Core_kernel
open Core
open Lib

type t =
  | DLit of Lit.Raw.t * int
  | DPred of Pred.t * int
[@@deriving compare, eq]

let to_node = function
  | DLit (lit, idx) -> Node.NLit (lit, idx)
  | DPred (pred, idx) -> Node.NPred (pred, idx)
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
    | DLit (lit, idx) ->
      Fmt.(
        hbox
        @@ prefix (always "literal ")
        @@ pair ~sep:(always "@") Lit.Raw.pp int)
        ppf
        (lit, idx)
    | DPred (pred, idx) ->
      Fmt.(
        hbox @@ prefix (always "literal ") @@ pair ~sep:(always "@") Pred.pp int)
        ppf
        (pred, idx)
  ;;

  let pp = `NoPrec pp
end)
