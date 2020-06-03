open Core_kernel
open Lib

module X = struct
  type t =
    | EffRandom
    | EffIO
  [@@deriving compare, sexp]

  let pp ppf = function
    | EffRandom -> Fmt.string ppf "Random"
    | EffIO -> Fmt.string ppf "IO"
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
module Set = Set.Make (X)
