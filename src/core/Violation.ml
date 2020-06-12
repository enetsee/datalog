open Core_kernel
open Lib
open Reporting

type t =
  { dest : Dataflow.Dest.t
  ; tmvar : Tmvar.t
  ; region : Region.t [@compare.ignore]
  }
[@@deriving compare, eq]

let violation ?(region = Region.empty) dest tmvar = { dest; tmvar; region }

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { dest; tmvar; _ } =
    Fmt.(hbox @@ pair ~sep:sp Dataflow.Dest.pp (quote Tmvar.pp))
      ppf
      (dest, tmvar)
  ;;

  let pp = `NoPrec pp
end)
