open Core_kernel
open Reporting
open Lib

module Err = struct
  type t =
    | RangeWildcard of Dataflow.Dest.t * Region.t
    | RangeViolations of RangeRepair.Violation.t list
    | NoCompatibleOrder of (Binding.t * Region.t) list
    | NegativeCycles of (Pred.t * Pred.t) list
end

module Warn = struct
  type t = UnusedClause of Region.t

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf = function
      | UnusedClause region ->
        Fmt.(suffix (any ": unused clause") Region.pp) ppf region
    ;;

    let pp = `NoPrec pp
  end)
end

module Topic = struct
  module X = struct
    type t = Warn.t list

    let mempty = []
    let append x y = x @ y
    let pp ppf xs = Fmt.(vbox @@ list ~sep:cut Warn.pp) ppf xs
    let pp = `NoPrec pp
  end

  include X
  include Monoid.Make0 (X)
  include Pretty.Make0 (X)
end

(** -- Dead code elimination ------------------------------------------------ *)
let elim_dead_clauses prog =
  let deps = Dependency.Raw.from_program prog in
  let dead = Dependency.Raw.dead_clauses deps prog in
  { prog with
    clauses =
      List.filteri ~f:(fun idx _ -> not @@ Int.Set.mem dead idx) prog.clauses
  }
;;

(** -- Range-restriction repair --------------------------------------------- *)
let repair_range_violation prog = RangeRepair.apply prog

(** -- Automatic subgoal scheduling / generalized adornment ----------------- *)
let generalized_adornment prog = Adorn.apply prog

(* -- Stratification -------------------------------------------------------- *)
let stratify prog =
  let queries = Program.Adorned.queries_of prog in
  Result.map ~f:(fun strata -> Program.Stratified.{ strata; queries })
  @@ Dependency.Adorned.(stratify @@ from_program prog)
;;
