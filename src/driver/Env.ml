open Core

type t =
  { reserved_names : Name.t list
  ; trg : Ty.TRG.t
  ; guard_prefix : string
  }
[@@deriving fields]

let default =
  { reserved_names = []; guard_prefix = "guard"; trg = Ty.TRG.empty }
;;
