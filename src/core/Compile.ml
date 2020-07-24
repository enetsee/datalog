open Core_kernel
open Reporting

module Make (M : CoreM.S) = struct
  include RangeRepair.Make (M)
  include Adorn.Make (M)
  include Typing.Make (M)

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
        @@ Set.to_list
        @@ Program.Raw.intensionals prog
      in
      set_typing_env @@ TypingEnv.add_preds tyenv ~preds
      >>= fun _ -> return prog)
  ;;

  let add_data data params =
    M.(
      get_typing_env
      >>= fun tyenv ->
      set_typing_env
      @@ TypingEnv.add_datas
           ~datas:
             (List.map data ~f:(fun (name, params) ->
                  name, TTC.ttc @@ List.map ~f:snd params))
      @@ TypingEnv.add_params ~params tyenv)
  ;;

  (** TODO: move this to `Source`?  *)
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

  let to_stratified
      Module.{ program; exports; knowledge = kb_in; tydefs; data; params }
    =
    M.(
      resolve_exports program exports
      >>= fun exports ->
      initialize_typing program
      >>= elim_dead_clauses ~exports
      >>= fix_program ~exports
      >>= fun (repaired, kb_repair) ->
      let kb = Knowledge.Base.union kb_in kb_repair in
      let trg = Ty.TRG.from_list tydefs in
      with_tydefs
        trg
        (add_data data params
        >>= fun _ ->
        typing_of_knowledge_base kb_repair
        >>= fun _ ->
        adorn_program repaired ~exports
        >>= stratify
        >>= fun stratified ->
        typing_of_program stratified
        >>= fun _ -> get_typing_env >>= fun env -> return (stratified, kb, env)
        ))
  ;;
end
