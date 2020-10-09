open Core_kernel
open Lib

module Minimal = struct
  type t =
    | Top
    | Symbol
    | Number
    | Bool
    | Real
    | Int
    | Date
    | Span
    | Named of Name.t
    | Bot
  [@@deriving eq, compare, sexp]

  let pp ppf = function
    | Top -> Fmt.string ppf "top"
    | Bot -> Fmt.string ppf "bottom"
    | Symbol -> Fmt.string ppf "symbol"
    | Named name -> Fmt.(any "@" ++ Name.pp) ppf name
    | Number -> Fmt.string ppf "number"
    | Int -> Fmt.string ppf "int"
    | Real -> Fmt.string ppf "real"
    | Bool -> Fmt.string ppf "bool"
    | Date -> Fmt.string ppf "date"
    | Span -> Fmt.string ppf "span"
  ;;

  let pp = `NoPrec pp
  let named str = Named (Name.from_string str)
  let named' n = Named n
end

include Minimal
include Pretty.Make0 (Minimal)
module Map = Map.Make (Minimal)
module Set = Set.Make (Minimal)
