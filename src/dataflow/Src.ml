open Core_kernel
open Lib

type t =
  | SLit of Core.Lit.Raw.t * int
  | SConst of Const.t
[@@deriving compare, eq]

let to_node = function
  | SLit (lit, idx) -> Node.NLit (lit, idx)
  | SConst const -> Node.NConst const
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
    | SLit (lit, idx) ->
      Fmt.(
        hbox
        @@ prefix (always "literal ")
        @@ pair ~sep:(always "@") Core.Lit.Raw.pp int)
        ppf
        (lit, idx)
    | SConst c -> Fmt.(prefix (always "constant ") Const.pp) ppf c
  ;;

  let pp = `NoPrec pp
end)
