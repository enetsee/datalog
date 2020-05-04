open Core_kernel
open Lib
open Reporting

module X = struct
  type 'a t =
    { predSym : Core.PredSymbol.t Located.t
    ; terms : 'a Located.t list
    }
  [@@deriving eq, compare]

  let atom predSym terms = {predSym;terms}
  
  let pp pp_a ppf { predSym; terms } =
    Fmt.(
      hovbox
      @@ pair (Located.pp Core.PredSymbol.pp)
      @@ parens
      @@ list ~sep:comma (Located.pp pp_a))
      ppf
      (predSym, terms)
  ;;

  let pp = `NoPrec pp

  let map t ~f = 
    { t with terms = List.map ~f:(Located.map ~f) t.terms}
end

include X
include Pretty.Make1 (X)
include Functor.Make1 (X)
