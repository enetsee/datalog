type t =
  { line : int
  ; col : int
  }
[@@deriving eq]

let empty = { line = -1; col = -1 }
