open Core_kernel
open Lib

module X = struct
  type 'a t =
    { annot : 'a Annotation.t
    ; pred : 'a Pred.t
    ; terms : Symbol.t list
    }

  let pp pp_a ppf { pred; terms; _ } =
    Fmt.(hovbox @@ pair (Pred.pp pp_a) @@ parens @@ list ~sep:comma Symbol.pp)
      ppf
      (pred, terms)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make1 (X)
