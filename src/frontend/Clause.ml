open Core_kernel
open Reporting
open Lib

module X = struct
  type t =
    { head : Core.Term.t Subgoal.t
    ; body : Core.Term.t Subgoal.t
    }
  [@@deriving eq, compare]

  let clause head body = {head;body}
  let pp ppf { head; body } =
    Fmt.(hovbox @@ pair ~sep:(any " :-@, ") (Subgoal.pp Core.Term.pp) (Subgoal.pp Core.Term.pp ++ any "."))
      ppf
      (head, body)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
