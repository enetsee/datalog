open Core_kernel

module type MonadCore = sig
  include Effect.MonadReader.S1
  include Adorn.MonadAdorn with type 'a t := 'a t
  include RangeRepair.MonadRepair with type 'a t := 'a t
  include Typing.MonadTyping with type 'a t := 'a t

  val get_typing_env : TypingEnv.t t
  val set_typing_env : TypingEnv.t -> unit t
  val err_neg_cycles : (Pred.t * Pred.t) list -> _ t
  val err_wildcards_in_head : Reporting.Region.t list -> _ t
  val err_empty_relations : Pred.t list -> _ t
  val warn_unused_clauses : Reporting.Region.t list -> unit t
end

module Make (M : MonadCore) = struct
  include RangeRepair.Make (M)
  include Adorn.Make (M)
  include Typing.Make (M)

  let check_wildcards_in_head prog =
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
    | [] -> M.return prog
    | _ -> M.err_wildcards_in_head errs
  ;;

  (** Eliminate unused clauses and issue warnings *)
  let elim_dead_clauses prog queries =
    M.(
      match Dependency.Raw.dead_clauses prog queries with
      | alive, [] -> return @@ Program.Raw.program alive
      | alive, dead ->
        map ~f:Fn.(const @@ Program.Raw.program alive)
        @@ warn_unused_clauses
        @@ List.map ~f:Clause.Raw.region_of dead)
  ;;

  let stratify prog =
    match Dependency.Adorned.(stratify @@ from_program prog) with
    | Ok strata -> M.return @@ Program.Stratified.{ strata }
    | Error cycles -> M.err_neg_cycles cycles
  ;;

  let initialize_typing prog =
    M.(
      get_typing_env
      >>= fun tyenv ->
      let preds =
        List.map ~f:(fun pr -> Pred.name_of pr, TypingEnv.empty_info)
        @@ Program.Raw.preds_of prog
      in
      set_typing_env @@ TypingEnv.add_preds tyenv ~preds
      >>= fun _ -> return prog)
  ;;

  let to_stratified _ = failwith ""

  (* Module.{clauses;knowledge;params;tydefs;data;exports} = *)
  (* M.(
     let prog = Program.Raw.program
     initialize_typing (prog,schema,params)
     >>= check_wildcards_in_head
     >>= elim_dead_clauses
     >>= fix_program
     >>= fun (prog, kb_repair) ->
     let kb = Knowledge.Base.union kb_in kb_repair in
     adorn_program prog
     >>= stratify
     >>= fun prog ->
     typing_of prog
     >>= fun _ ->
     get_typing_env
     >>= fun env ->
     return (prog, kb, env)
     ) *)
end
