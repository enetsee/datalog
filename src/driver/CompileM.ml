open Core_kernel
open Core
include Effect.MonadRWSFail.Make (Err) (Topic) (State) (Env)

(* -- Errors ---------------------------------------------------------------- *)

let err_bad_parse_state () = fail Err.bad_parse_state
let err_parse_err ~msg ~region = fail @@ Err.parse_error msg region
let err_file_not_found path = fail @@ Err.file_not_found path
let err_reserved_word region = fail @@ Err.reserved_word region
let err_name_shadowed name region = fail @@ Err.name_shadowed name region
let err_head_not_atom region = fail @@ Err.head_not_atom region
let err_fact_has_wildcard region = fail @@ Err.fact_has_wildcard region

let err_fact_not_range_restricted region =
  fail @@ Err.fact_not_range_restricted region
;;

let err_clause_neg region = fail @@ Err.clause_neg region
let err_clause_disj region = fail @@ Err.clause_disj region
let err_clause_null_op region = fail @@ Err.clause_null_op region
let err_no_ordering bpatt region = fail @@ Err.no_well_moded_order bpatt region
let err_range_violations vs = fail @@ Err.range_violations vs
let err_neg_cycles cycles = fail @@ Err.negative_cycles cycles

let err_unresolved_export name region =
  fail @@ Err.unresolved_export name region
;;

let err_unbound_param name region = fail @@ Err.unbound_param name region
let err_empty_relations ps = fail @@ Err.empty_relations ps

(* -- Writer helpers -------------------------------------------------------- *)

let warn w = tell [ w ]
let warns ws = tell ws
let warn_unused_clauses cls = warns @@ List.map ~f:Warn.unused_clause cls
let warn_no_exports () = warn Warn.no_exports

(* -- Reader helpers -------------------------------------------------------- *)

let subtypes_of ty =
  map ~f:(fun trg ->
      Option.value ~default:Ty.Set.empty @@ Core.Ty.TRG.subtypes_of ~ty trg)
  @@ reader Env.trg
;;

let with_tydefs trg = local ~f:(fun env -> { env with trg })

(* -- State helpers --------------------------------------------------------- *)

let is_name_already_bound name =
  map ~f:(fun ty_env ->
      Option.value_map ~default:false ~f:Fn.(const true)
      @@ TypingEnv.find_name ty_env ~name)
  @@ gets State.typing_env
;;

let get_typing_of name =
  map ~f:(fun ty_env ->
      match TypingEnv.find_pred_typing ~name ty_env with
      | Some typing -> typing
      | _ ->
        Option.value_map ~default:Typing.bottom ~f:Typing.singleton
        @@ TypingEnv.find_data ~name ty_env)
  @@ gets State.typing_env
;;

let set_typing_of name typing =
  gets State.typing_env
  >>= fun ty_env ->
  modify (fun st ->
      { st with
        typing_env = TypingEnv.update_pred_typing_exn ty_env ~name ~typing
      })
;;

let incr =
  gets (fun State.{ fresh_src; _ } -> fresh_src)
  >>= fun ctr ->
  modify (fun st -> { st with fresh_src = ctr + 1 }) >>= fun _ -> return ctr
;;

(* -- State: typing helpers ------------------------------------------------- *)
let get_typing_env = gets (fun State.{ typing_env; _ } -> typing_env)
let set_typing_env typing_env = modify (fun st -> { st with typing_env })

let get_param_ty name =
  get_typing_env >>= fun env -> return @@ TypingEnv.find_param env ~name
;;

let get_pred_info name =
  get_typing_env >>= fun env -> return @@ TypingEnv.find_pred env ~name
;;

let get_pred_constraint name =
  map
    ~f:
      (Option.value_map
         ~default:Constraint.trivial
         ~f:TypingEnv.(fun { cstr; _ } -> cstr))
  @@ get_pred_info name
;;

let set_pred_constraint name cstr =
  get_typing_env
  >>= fun env ->
  set_typing_env @@ TypingEnv.update_pred_constraint_exn env ~name ~cstr
;;

let get_pred_nature name =
  map
    ~f:
      Option.(
        value_map
          ~default:Nature.Logical
          ~f:TypingEnv.(fun { nature; _ } -> nature))
  @@ get_pred_info name
;;

let get_pred_effects name = map ~f:Nature.effects_of @@ get_pred_nature name
let guard_prefix = map ~f:(fun Env.{ guard_prefix; _ } -> guard_prefix) ask

let fresh_predsym pfx =
  incr >>= fun i -> return Name.(from_string @@ pfx ^ string_of_int i)
;;

let fresh_guardsym = guard_prefix >>= fresh_predsym
