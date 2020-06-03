open Lib

type t =
  | Free
  | Bound
[@@deriving eq, compare, sexp]

let well_moded t ~mode =
  match t, mode with
  | _, Mode.Opt | Bound, Req -> true
  | Free, Req -> false
;;

let pp ppf = function
  | Free -> Fmt.char ppf 'f'
  | Bound -> Fmt.char ppf 'b'
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp = `NoPrec pp
end)
