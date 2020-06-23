open Core_kernel
open Lib

module X = struct
  type t =
    | SText of string
    | SReal of string
    | SInt of int
    | SBool of bool
    | SNull
  [@@deriving eq, compare, hash, sexp]

  let pp ppf = function
    | SText s -> Fmt.string ppf s
    | SReal s -> Fmt.string ppf s
    | SInt n -> Fmt.int ppf n
    | SBool b -> Fmt.bool ppf b
    | SNull -> Fmt.string ppf "null"
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

let from_string s = SText s
let from_int n = SInt n
let from_bool n = SBool n
let null = SNull
