open Core_kernel
open Core

type t =
  { reserved_names : Name.t list
  ; trg : TRG.t
  ; guard_prefix : string
  }
[@@deriving fields]

let default = { reserved_names = []; guard_prefix = "guard"; trg = TRG.empty }
