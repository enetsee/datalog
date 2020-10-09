open Core_kernel
open Parser
open Core
open Reporting
module P = Parse.Make (CompileM)
module S = Source.Program.Make (CompileM)
module DF = Dataflow.Repair.Make (CompileM)
module A = Adorn.Make (CompileM)
module T = Typecheck.Typing.Make (CompileM)

(** Eliminate unused clauses and issue warnings *)
let elim_dead_clauses prog ~exports =
  CompileM.(
    match Dependency.Raw.dead_clauses prog exports with
    | alive, [] -> return @@ Program.Raw.program alive
    | alive, dead ->
      map ~f:Fn.(const @@ Program.Raw.program alive)
      @@ warn_unused_clauses
      @@ List.map ~f:Clause.Raw.region_of dead)
;;

let stratify prog =
  match Dependency.Adorned.(stratify @@ from_program prog) with
  | Ok strata -> CompileM.return @@ Program.Stratified.{ strata }
  | Error cycles -> CompileM.err_neg_cycles cycles
;;

let initialize_typing prog =
  CompileM.(
    get_typing_env
    >>= fun tyenv ->
    let preds =
      List.map ~f:(fun pr -> Pred.name_of pr, Typecheck.TypingEnv.empty_info)
      @@ Set.to_list
      @@ Program.Raw.intensionals prog
    in
    set_typing_env @@ Typecheck.TypingEnv.add_preds tyenv ~preds
    >>= fun _ -> return prog)
;;

let add_data data params =
  CompileM.(
    get_typing_env
    >>= fun tyenv ->
    set_typing_env
    @@ Typecheck.TypingEnv.add_datas
         ~datas:
           (List.map data ~f:(fun (name, params) ->
                name, TTC.ttc @@ List.map ~f:snd params))
    @@ Typecheck.TypingEnv.add_params ~params tyenv)
;;

(** TODO: move this to `Source`?  *)
let resolve_exports program exports =
  let lut =
    Name.Map.of_alist_exn
    @@ List.map ~f:(fun pred -> Pred.name_of pred, pred)
    @@ Program.Raw.preds_of program
  in
  CompileM.(
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
  CompileM.(
    resolve_exports program exports
    >>= fun exports ->
    initialize_typing program
    >>= elim_dead_clauses ~exports
    >>= DF.fix_program ~exports
    >>= fun (repaired, kb_repair) ->
    let kb = Knowledge.Base.union kb_in kb_repair in
    let trg = TRG.from_list tydefs in
    with_tydefs
      trg
      (add_data data params
      >>= fun _ ->
      T.typing_of_knowledge_base kb_repair
      >>= fun _ ->
      A.adorn_program repaired ~exports
      >>= stratify
      >>= fun stratified ->
      T.typing_of_program stratified
      >>= fun _ -> get_typing_env >>= fun env -> return (stratified, kb, env)))
;;

let compile file =
  try
    let res, warn, _ =
      CompileM.(
        run ~env:Env.default ~st:State.default
        @@ (P.parse_file file >>= S.to_core >>= to_stratified))
    in
    match res with
    | Ok (prog, kb, env) -> Ok (prog, kb, env, Topic.to_string warn)
    | Error err -> Error (Err.to_string err)
  with
  | exn -> Error (Exn.to_string exn)
;;
