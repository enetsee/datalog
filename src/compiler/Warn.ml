open Reporting
open Lib

module X = struct
  type t =
    | Unused_Clause of Region.t
    | No_Exports
  [@@deriving variants]

  let pp ppf = function
    | Unused_Clause region ->
      Fmt.(suffix (any ": unused clause") Region.pp) ppf region
    | No_Exports -> Fmt.string ppf "This program exports no predicates"
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
