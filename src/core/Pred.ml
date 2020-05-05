open Core_kernel
open Lib

module X = struct
  type 'a t =
    { predSym : PredSymbol.t
    ; arity : int
    ; nature : Nature.t
    ; annot : 'a Annotation.t [@compare.ignore]
    }
  [@@deriving compare]

  let equal
      _eq_a
      { predSym = p1; arity = t1; _ }
      { predSym = p2; arity = t2; _ }
    =
    PredSymbol.equal p1 p2 && t1 = t2
  ;;

  let pp _pp_a ppf { predSym; _ } = PredSymbol.pp ppf predSym
  let pp = `NoPrec pp
end

include X
include Pretty.Make1 (X)
