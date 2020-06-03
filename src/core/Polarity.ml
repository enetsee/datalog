open Lib

type t =
  | Pos
  | Neg
[@@deriving eq, compare, sexp]

let toggle = function
  | Pos -> Neg
  | Neg -> Pos
;;

let isPos = function
  | Pos -> true
  | _ -> false
;;

let isNeg t = not @@ isPos t

let pp_verbose ppf = function
  | Pos -> Fmt.string ppf "Pos"
  | Neg -> Fmt.string ppf "Neg"
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
    | Pos -> ()
    | Neg -> Fmt.char ppf '!'
  ;;

  let pp = `NoPrec pp
end)
