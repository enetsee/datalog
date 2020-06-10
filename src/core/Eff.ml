open Core_kernel
open Lib

module X = struct
  type t =
    | EffRandom
    | EffHTTP
    | EffRW
  [@@deriving eq, compare, hash, sexp]

  let pp ppf = function
    | EffRandom -> Fmt.string ppf "RND"
    | EffRW -> Fmt.string ppf "RW"
    | EffHTTP -> Fmt.string ppf "HTTP"
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
module Set = Set.Make (X)
