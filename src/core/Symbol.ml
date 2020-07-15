open Core_kernel
open Lib

type t =
  | SText of string
  | SReal of string
  | SInt of int
  | SBool of bool
  | SDate of Date.t
  | SSpan of Time.Span.t
[@@deriving eq, compare, sexp,variants]

let text s = SText s
let int n = SInt n
let real f = SReal (string_of_float f)
let bool n = SBool n

let date d = SDate d

let span uot = SSpan (Time.Span.of_unit_of_time uot)

let type_of = function
  | SText _ -> Ty.Symbol
  | SReal _ -> Ty.Real
  | SInt _ -> Ty.Int
  | SBool _ -> Ty.Bool
  | SDate _ -> Ty.Date
  | SSpan _ -> Ty.Span
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
    | SText s -> Fmt.string ppf s
    | SReal s -> Fmt.string ppf s
    | SInt n -> Fmt.int ppf n
    | SBool b -> Fmt.bool ppf b
    | SDate d -> Date.pp ppf d
    | SSpan s -> Time.Span.pp ppf s
  ;;

  let pp = `NoPrec pp
end)
