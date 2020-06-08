open Core_kernel

let apply prog =
  Raw.(
    let deps = Dependency.from_program prog in
    let dead = Dependency.dead_clauses deps prog in
    { prog with
      clauses =
        List.filteri ~f:(fun idx _ -> not @@ Int.Set.mem dead idx) prog.clauses
    })
;;
