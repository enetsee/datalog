open Lib
open Reporting

type t = PredSymClash of Region.t

let pp ppf = function
  | PredSymClash region ->
    Fmt.(
      any "Predicate symbol clashes with a reserved name " ++ parens Region.pp)
      ppf
      region
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp = `NoPrec pp
end)
