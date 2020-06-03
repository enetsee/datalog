open Core_kernel
open Lib
open Reporting

module Minimal = struct
  type t =
    { pred : Pred.t
    ; terms : Symbol.t list
    ; region : Region.t [@compare.ignore]
    }
  [@@deriving compare, sexp]

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
module Set = Set.Make (Minimal)
