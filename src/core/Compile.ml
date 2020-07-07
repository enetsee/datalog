open Core_kernel

module type MonadCore = sig
  include Adorn.MonadAdorn
  include RangeRepair.MonadRepair with type 'a t := 'a t
  include Typing.MonadTyping with type 'a t := 'a t

  val get_typing_env : TypingEnv.t t
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

  let elim_dead_clauses prog =
    M.(
      match Dependency.Raw.dead_clauses prog with
      | alive, [] -> return Program.Raw.{ prog with clauses = alive }
      | alive, dead ->
        warn_unused_clauses dead
        >>= fun _ -> return Program.Raw.{ prog with clauses = alive })
  ;;

  let stratify (Program.Adorned.{ queries; data; params; _ } as prog) =
    match Dependency.Adorned.(stratify @@ from_program prog) with
    | Ok strata -> M.return Program.Stratified.{ strata; queries; data; params }
    | Error cycles -> M.err_neg_cycles cycles
  ;;

  let to_stratified (prog, kb_in) =
    M.(
      prog
      |> check_wildcards_in_head
      >>= elim_dead_clauses
      >>= fix_program
      >>= fun (prog, kb_repair) ->
      let kb = Knowledge.Base.union kb_in kb_repair in
      adorn_program prog
      >>= stratify
      >>= fun prog ->
      typing_of prog
      >>= fun _ -> get_typing_env >>= fun env -> return (prog, kb, env))
  ;;
end
