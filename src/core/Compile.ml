open Core_kernel
open Reporting

module type MonadCore = sig
  include Adorn.MonadAdorn
  include RangeRepair.MonadRepair with type 'a t := 'a t
  include Typing.MonadTyping with type 'a t := 'a t

  val with_tydefs : Ty.TRG.t -> 'a t -> 'a t
  val get_typing_env : TypingEnv.t t
  val set_typing_env : TypingEnv.t -> unit t
  val err_neg_cycles : (Pred.t * Pred.t) list -> _ t
  val err_unresolved_export : Name.t -> Region.t -> _ t
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
              | Term.TWild(_,region) -> Some region
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
  let elim_dead_clauses prog ~exports =
    M.(
      match Dependency.Raw.dead_clauses prog exports with
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

  (** TODO: move this to `Source` *)
  let resolve_exports program exports =
    let lut =
      Name.Map.of_alist_exn
      @@ List.map ~f:(fun pred -> Pred.name_of pred, pred)
      @@ Program.Raw.preds_of program
    in
    M.(
      all
      @@ List.map exports ~f:(fun nmloc ->
             let nm = Located.elem_of nmloc in
             match Name.Map.find lut nm with
             | Some pred -> return pred
             | _ -> err_unresolved_export nm @@ Located.region_of nmloc))
  ;;

  let to_stratified Module.{ program; exports; knowledge = kb_in; tydefs; _ } =
    M.(
      resolve_exports program exports
      >>= fun exports ->
      initialize_typing program
      >>= check_wildcards_in_head
      >>= elim_dead_clauses ~exports
      >>= fix_program ~exports
      >>= fun (repaired, kb_repair) ->
      let kb = Knowledge.Base.union kb_in kb_repair in
      adorn_program repaired ~exports
      >>= stratify
      >>= fun stratified ->
      let trg = Ty.TRG.from_list tydefs in
      with_tydefs trg @@ typing_of stratified
      >>= fun _ -> get_typing_env >>= fun env -> return (stratified, kb, env))
  ;;
end
