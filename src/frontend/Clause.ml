open Core_kernel
open Reporting
open Lib

module X = struct
  type t =
    { head : Subgoal.t
    ; body : Subgoal.t
    }
  [@@deriving eq, compare]

  let pp ppf { head; body } =
    Fmt.(hovbox @@ pair ~sep:(any " :-@, ") Subgoal.pp (Subgoal.pp ++ any "."))
      ppf
      (head, body)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
