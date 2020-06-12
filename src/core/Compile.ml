open Core_kernel

let wildcards_in_head prog =
  let errs =
    List.concat_map ~f:(fun cl ->
        List.filter_map ~f:(function
            | Term.TWild region -> Some region
            | _ -> None)
        @@ Lit.Raw.terms_of
        @@ Clause.Raw.head_of cl)
    @@ Program.Raw.clauses_of prog
  in
  match errs with
  | [] -> MonadCompile.return prog
  | _ -> MonadCompile.(fail Err.(WildcardsInHead errs))
;;

let elim_dead_clauses prog =
  MonadCompile.(
    let deps = Dependency.Raw.from_program prog in
    let dead_idxs = Dependency.Raw.dead_clauses deps prog in
    let alive, dead =
      List.partition_map ~f:(fun (idx, cl) ->
          if Int.Set.mem dead_idxs idx
          then `Snd (Warn.UnusedClause (Clause.Raw.region_of cl))
          else `Fst cl)
      @@ List.mapi ~f:Tuple2.create
      @@ Program.Raw.clauses_of prog
    in
    warns dead >>= fun _ -> return { prog with clauses = alive })
;;

let stratify (Program.Adorned.{ queries; _ } as prog) =
  match Dependency.Adorned.(stratify @@ from_program prog) with
  | Ok strata -> MonadCompile.return Program.Stratified.{ strata; queries }
  | Error cycles -> MonadCompile.(fail Err.(NegativeCycles cycles))
;;

let to_stratified (prog, kb) =
  MonadCompile.(
    prog
    |> wildcards_in_head
    >>= elim_dead_clauses
    >>= RangeRepair.apply
    >>= fun (prog, kb') ->
    Adorn.adorn_program prog
    >>= stratify
    |> map ~f:(fun prog -> prog, Knowledge.Base.union kb kb'))
;;
