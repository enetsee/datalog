open Core_kernel

(** -- Dead code elimination ------------------------------------------------ *)
let elim_dead_clauses prog = DeadClause.apply prog

(** -- Range-restriction repair --------------------------------------------- *)
let repair_range_violation prog = RangeRepair.apply prog

(** -- Automatic subgoal scheduling / generalized adornment ----------------- *)
let generalized_adornment prog = GenAdorn.apply prog

(* -- Stratification -------------------------------------------------------- *)
let stratify prog =
  let queries = Adorned.Program.queries_of prog in
  Result.map ~f:(fun strata -> Stratified.{ strata; queries })
  @@ Adorned.Dependency.(stratify @@ from_program prog)
;;
