open Core_kernel
open Lib

module X = struct
  type t = { name : string } [@@deriving eq, compare, sexp]

  let pp ppf { name } = Fmt.string ppf name
  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
module Map = Map.Make (X)

let from_string name = { name }
