open Core_kernel
open Core
open Lib

type t =
  | CWild of Name.t option
  | CSym of Symbol.t
  | CParam of Name.t
[@@deriving compare, eq, sexp]

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
    | CSym sym -> Symbol.pp ppf sym
    | CParam nm -> Fmt.(any "#" ++ Name.pp) ppf nm
    | CWild (Some nm) -> Fmt.(any "_" ++ Name.pp) ppf nm
    | CWild _ -> Fmt.char ppf '_'
  ;;

  let pp = `NoPrec pp
end)
