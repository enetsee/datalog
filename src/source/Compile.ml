open Core_kernel

let generate_query_head t = Program.mapM ~f:Statement.generate_query_head t
let set_nature t = Program.mapM ~f:Statement.set_nature t

let normalize Program.{ stmts } =
  Program.{ stmts = List.concat_map ~f:Statement.normalize stmts }
;;

let collect_reprs rs =
  let rec aux cls qrys fcts = function
    | [] -> cls, qrys, fcts
    | Statement.RCls cl :: rest -> aux (cl :: cls) qrys fcts rest
    | RQry qry :: rest -> aux cls (qry :: qrys) fcts rest
    | RKnw fct :: rest -> aux cls qrys (fct :: fcts) rest
    | RDcl :: rest -> aux cls qrys fcts rest
  in
  aux [] [] [] rs
;;

let mk_prog reprs =
  MonadCompile.(
    constraints
    >>= fun cstrs ->
    let cls, qrys, fcts = collect_reprs reprs in
    let queries =
      List.map ~f:(fun Core.Clause.Raw.{ head = { pred; _ }; _ } -> pred) qrys
    and clauses = cls @ qrys in
    let prog =
      ( Core.Program.Raw.(program ~cstrs clauses queries)
      , Core.Knowledge.Base.of_list fcts )
    in
    if List.is_empty queries
    then warn Warn.NoQueries >>= fun _ -> return prog
    else return prog)
;;

let to_core t =
  MonadCompile.(
    generate_query_head t
    >>= set_nature
    >>= fun t ->
    let Program.{ stmts } = normalize t in
    List.map ~f:Statement.to_core stmts |> all >>= mk_prog)
;;
