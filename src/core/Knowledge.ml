open Core_kernel
open Lib
open Reporting

type t =
  { pred : Pred.t
  ; terms : Symbol.t list
  ; region : Region.t
  }

let knowledge ?(region = Region.empty) pred terms = { pred; terms; region }

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { pred; terms; _ } =
    Fmt.(hbox @@ pair Pred.pp @@ parens @@ hovbox @@ list ~sep:comma Symbol.pp)
      ppf
      (pred, terms)
  ;;

  let pp = `NoPrec pp
end)
