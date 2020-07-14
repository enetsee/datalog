open Core_kernel
open Lib
open Reporting

module Minimal = struct
  type t =
    { pred : Pred.t
    ; terms : Symbol.t list
    ; region : Region.t [@compare.ignore] [@equal.ignore]
    }
  [@@deriving eq, compare, sexp]

  let knowledge ?(region = Region.empty) pred terms = { pred; terms; region }

  let pp ppf { pred; terms; _ } =
    Fmt.(hbox @@ pair Pred.pp @@ parens @@ hovbox @@ list ~sep:comma Symbol.pp)
      ppf
      (pred, terms)
  ;;

  let pp = `NoPrec pp
end

include Minimal
include Pretty.Make0 (Minimal)

module Base = struct
  include Set.Make (Minimal)

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf kb = Fmt.(vbox @@ list ~sep:cut pp) ppf @@ elements kb
    let pp = `NoPrec pp
  end)
end
