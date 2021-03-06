open Core_kernel

type t =
  { line : int
  ; col : int
  }
[@@deriving eq, sexp]

let empty = { line = -1; col = -1 }
let is_empty { line; col } = line = -1 && col = -1
