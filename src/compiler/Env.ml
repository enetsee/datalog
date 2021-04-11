open Core_kernel

type t =
  { reserved_names : Name.t list
  ; trg : Type.TRG.t
  ; guard_prefix : string
  }
[@@deriving fields]

let default =
  { reserved_names = []; guard_prefix = "guard"; trg = Type.TRG.empty }
;;
