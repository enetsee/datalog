open Core_kernel
open Reporting
open Lib

module X = struct
  type t =
    { head : Core.Tmvar.t Subgoal.t option
    ; body : Core.Term.t Subgoal.t
    }
  [@@deriving eq, compare]

  let query ?(head = None) body = { head; body }

  let pp ppf { head; body } =
    match head with
    | None ->
      Fmt.(hovbox @@ (any "?-@, " ++ Subgoal.pp Core.Term.pp ++ any "."))
        ppf
        body
    | Some head ->
      Fmt.(
        hovbox
        @@ pair
             ~sep:(any " ?-@, ")
             (Subgoal.pp Core.Tmvar.pp)
             (Subgoal.pp Core.Term.pp ++ any "."))
        ppf
        (head, body)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

(* -- Query ----------------------------------------------------------------- *)

let atoms { head; body } =
  Option.value_map head ~default:[] ~f:(fun sg ->
      List.map ~f:(Atom.map ~f:Core.Term.var') @@ Subgoal.atoms sg)
  @ Subgoal.atoms body
;;

let tmvars { body; _ } = Subgoal.tmvars Core.Term.tmvars body

(* -- Core translation ------------------------------------------------------ *)

(** Convert a `Query` to a `Clause` by lifting term-variables into terms
    whilst guarding against the possibility that no head has been generated
    for the query  
*)
let to_clause { head; body } =
  Logger.(
    match head with
    | Some head ->
      return @@ Clause.{ head = Subgoal.map ~f:Core.Term.var' head; body }
    | _ -> fail @@ query_not_named @@ Located.region_of @@ Subgoal.proj body)
;;

(** Translate a `Query` to a `Core` `Literal` via translation to `Clause` *)
let to_core t = Logger.(to_clause t >>= Clause.to_core)
