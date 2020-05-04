open Core_kernel
open Reporting
open Lib

module X = struct
  type t = { body : Subgoal.t } [@@deriving eq, compare]

  let pp ppf { body } =
    Fmt.(hovbox @@ (any " ?-@, " ++ Subgoal.pp ++ any ".")) ppf body
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
