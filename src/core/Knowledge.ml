open Core_kernel
open Lib
open Reporting

module KTerm = struct
  module Minimal = struct
    (** Terms in literal knowledge bases may be either symbols or parameters *)
    type t = 
      | KSymbol of Symbol.t 
      | KParam of Name.t 
      [@@deriving eq, compare,sexp, variants]

    let pp ppf = function 
      | KSymbol s -> Symbol.pp ppf s 
      | KParam nm -> Fmt.(any "#" ++ Name.pp) ppf nm

    let pp = `NoPrec pp
  end

  include Minimal 
  include Pretty.Make0(Minimal)
end

module Minimal = struct
  type t =
    { pred : Pred.t
    ; terms : KTerm.t list
    ; region : Region.t [@compare.ignore] [@equal.ignore]
    }
  [@@deriving eq, compare, sexp]

  let knowledge ?(region = Region.empty) pred terms = { pred; terms; region }

  let pp ppf { pred; terms; _ } =
    Fmt.(hbox @@ pair Pred.pp @@ parens @@ hovbox @@ list ~sep:comma KTerm.pp)
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
