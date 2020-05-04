open Core_kernel
open Reporting
open Lib

module X = struct
  type t = { head : Subgoal.t } [@@deriving eq, compare]

  let pp ppf { head } = Fmt.(Subgoal.pp ++ any ".") ppf head
  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
