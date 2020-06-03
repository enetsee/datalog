open Core_kernel
open Lib
open Reporting

module Raw = struct
  type t =
    { pol : Polarity.t
    ; pred : Pred.t
    ; terms : Term.t list
    ; region : Region.t
    }

  let lit ?(pol = Polarity.Pos) ?(region = Region.empty) pred terms =
    { pol; pred; terms; region }
  ;;

  let neg t = { t with pol = Polarity.toggle t.pol }
  let effects_of { pred; _ } = Pred.effects_of pred
  let terms_of { terms; _ } = terms
  let vars_of { terms; _ } = List.concat_map ~f:Term.vars_of terms

  let pp ppf { pol; pred; terms; _ } =
    Fmt.(
      hbox
      @@ pair Polarity.pp
      @@ pair Pred.pp
      @@ parens
      @@ list ~sep:comma Term.pp)
      ppf
      (pol, (pred, terms))
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)
end

module Adorned = struct
  type t =
    { pol : Polarity.t
    ; bpatt : BindingPatt.t
    ; pred : Pred.t
    ; terms : Term.t list
    ; region : Region.t
    }
end
