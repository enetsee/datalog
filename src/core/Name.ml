open Core_kernel
open Lib

module X = struct
  type t = { name : string } [@@deriving eq, hash, compare, sexp]

  let from_string name = { name }
  let to_string { name } = name
  let pp ppf { name } = Fmt.string ppf name
  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
module Map = Map.Make (X)
module Set = Set.Make (X)
