open Lib

type t =
  | Opt
  | Req
[@@deriving compare, sexp]

(* -- Partial order  ---------------------------------------------------------*)

let leq t1 t2 =
  match t1, t2 with
  | Opt, _ | Req, Req -> true
  | _ -> false
;;

let max t1 t2 = if leq t1 t2 then t2 else t1
let min t1 t2 = if leq t1 t2 then t1 else t2

(* -- Pretty printing  -------------------------------------------------------*)

let pp ppf = function
  | Req -> Fmt.char ppf '+'
  | Opt -> Fmt.char ppf '?'
;;

let pp_silent ppf = function
  | Req -> Fmt.char ppf '+'
  | _ -> ()
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp = `NoPrec pp
end)
