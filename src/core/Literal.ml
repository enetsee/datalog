open Core_kernel
open Lib

module X = struct
  type 'a t =
    { polarity : Polarity.t
    ; pred : 'a Pred.t
    ; terms : Term.t list
    ; annot : 'a Annotation.t [@compare.ignore]
    }
  [@@deriving compare]

  let pp pp_a ppf { polarity; pred; terms; _ } =
    Fmt.(
      hbox
      @@ pair Polarity.pp
      @@ pair (Pred.pp pp_a)
      @@ parens
      @@ list ~sep:comma Term.pp)
      ppf
      (polarity, (pred, terms))
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make1 (X)

let toggle_polarity t = { t with polarity = Polarity.toggle t.polarity }
