open Core_kernel
open Reporting
open Lib

module X = struct
  type t = 
    { head : Core.Tmvar.t Subgoal.t option
    ; body : Core.Term.t Subgoal.t 
    } [@@deriving eq, compare]

  let query ?(head = None) body = { head ; body }
  let pp ppf { head;body } =
    match head with 
    | None -> 
      Fmt.(hovbox @@ (any " ?-@, " ++ Subgoal.pp Core.Term.pp ++ any ".")) ppf body

    | Some head -> 
      Fmt.(hovbox @@ pair ~sep:(any " ?-@, ") 
        (Subgoal.pp Core.Tmvar.pp)
        (Subgoal.pp Core.Term.pp ++ any ".")) ppf (head,body)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
