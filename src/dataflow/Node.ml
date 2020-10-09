open Core_kernel
open Core
open Lib

module X = struct
  type t =
    | NNull
    | NConst of Const.t
    | NPred of Pred.t * int
    | NLit of Lit.Raw.t * int
  [@@deriving compare, eq, sexp]

  let pp ppf = function
    | NNull -> Fmt.string ppf "null"
    | NConst const -> Fmt.(prefix (always "constant: ") Const.pp) ppf const
    | NPred (pr, idx) ->
      Fmt.(hbox @@ prefix (always "pred: ") @@ pair ~sep:comma Pred.pp int)
        ppf
        (pr, idx)
    | NLit (lit, idx) ->
      Fmt.(hbox @@ prefix (always "lit: ") @@ pair ~sep:comma Lit.Raw.pp int)
        ppf
        (lit, idx)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
module Map = Map.Make (X)
