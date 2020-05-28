open Core_kernel
open Lib

module X = struct
  type t =
    | Neg
    | Conj
    | Disj
  [@@deriving eq, compare]

  let pp ppf = function
    | Neg -> Fmt.(string ++ sp) ppf "not"
    | Conj -> Fmt.char ppf ','
    | Disj -> Fmt.char ppf ';'
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

let precedence = function
  | Neg -> 3
  | Conj -> 2
  | Disj -> 1
;;
