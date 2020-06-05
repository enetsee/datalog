open Core_kernel

let apply prog =
  Raw.(
    let deps = Dependency.from_program prog in
    let dead_preds = Dependency.dead_preds deps prog in
    let clauses =
      List.filter ~f:Fn.(compose (Pred.Set.mem dead_preds) Clause.head_pred_of)
      @@ Program.clauses_of prog
    in
    { prog with clauses })
;;
