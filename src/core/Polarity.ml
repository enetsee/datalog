open Lib

module X = struct
  type t =
    | Pos
    | Neg
  [@@deriving eq, compare]

  let pp ppf = function
    | Pos -> ()
    | Neg -> Fmt.char ppf '!'
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

let toggle = function
  | Pos -> Neg
  | Neg -> Pos
;;
