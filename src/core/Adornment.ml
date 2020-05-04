open Lib

module X = struct
  type t =
    | Free
    | Bound
  [@@deriving eq, compare, show]

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
