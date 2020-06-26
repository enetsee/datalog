open Core_kernel
open Lib

type t =
  | SText of string
  | SReal of string
  | SInt of int
  | SBool of bool
[@@deriving eq, compare, hash, sexp]

let from_string s = SText s
let from_int n = SInt n
let from_float f = SReal (string_of_float f)
let from_bool n = SBool n

let type_of = function
  | SText _ -> Ty.Symbol
  | SReal _ -> Ty.Real
  | SInt _ -> Ty.Int
  | SBool _ -> Ty.Bool
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
    | SText s -> Fmt.string ppf s
    | SReal s -> Fmt.string ppf s
    | SInt n -> Fmt.int ppf n
    | SBool b -> Fmt.bool ppf b
  ;;

  let pp = `NoPrec pp
end)
