open Core_kernel

let apply prog =
  Raw.(
    let deps = Dependency.from_program prog in
    let live_idxs = Dependency.live_clause_idxs deps prog in
    { prog with
      clauses =
        List.filteri ~f:(fun idx _ -> Int.Set.mem live_idxs idx) prog.clauses
    })
;;
