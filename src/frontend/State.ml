open Core_kernel

type t =
  { prefix : string option
  ; reserved : String.Set.t
  ; warnings : Warning.t list
  ; counter : int
  }

let init prefix reserved = { prefix; reserved; warnings = []; counter = 0 }
