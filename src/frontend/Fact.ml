open Core_kernel
open Reporting
open Lib

module X = struct
  type t = { head : Core.Term.t Subgoal.t } [@@deriving eq, compare]

  let fact head = { head }
  let pp ppf { head } = Fmt.(Subgoal.pp Core.Term.pp ++ any ".") ppf head
  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

(* -- Query ----------------------------------------------------------------- *)

let atoms { head } = Subgoal.atoms head
let tmvars { head } = Subgoal.tmvars Core.Term.tmvars head
