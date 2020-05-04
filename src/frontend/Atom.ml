open Core_kernel
open Lib
open Reporting

module X = struct
  type t =
    { predSym : Core.PredSymbol.t Located.t
    ; terms : Core.Term.t Located.t list
    }
  [@@deriving eq, compare]

  let pp ppf { predSym; terms } =
    Fmt.(
      hovbox
      @@ pair (Located.pp Core.PredSymbol.pp)
      @@ parens
      @@ list ~sep:comma (Located.pp Core.Term.pp))
      ppf
      (predSym, terms)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
