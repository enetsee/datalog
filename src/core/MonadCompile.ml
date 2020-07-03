open Core_kernel
open Reporting
open Lib

module Err = struct
  type t =
    | WildcardsInHead of Region.t list
    | RangeViolations of Violation.t list
    | NoCompatibleOrder of (Binding.t * Region.t)
    | NegativeCycles of (Pred.t * Pred.t) list
  [@@deriving eq]

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp_violation ppf Violation.{ tmvar; region; _ } =
      Fmt.(
        hovbox
        @@ pair
             ~sep:sp
             (prefix (any "Term variable ") @@ quote Tmvar.pp)
             (suffix (any " is not range restricted.")
             @@ prefix (any " at ") Region.pp))
        ppf
        (tmvar, region)
    ;;

    let pp_no_order ppf (bp, region) =
      Fmt.(
        hovbox
        @@ pair
             ~sep:sp
             (prefix
                (any
                   "No ordering of the clause is compatible with the binding \
                    pattern ")
             @@ quote Binding.pp)
             (prefix (any " at ") Region.pp))
        ppf
        (bp, region)
    ;;

    let pp_cycle ppf (src, dest) =
      Fmt.(
        hovbox
        @@ pair
             ~sep:sp
             (prefix (any "Negative cycle between predicate") @@ quote Pred.pp)
             (prefix (any "and predicate ") @@ quote Pred.pp))
        ppf
        (src, dest)
    ;;

    let pp_wildcard =
      Fmt.(
        hovbox
        @@ prefix
             (any "Wildcards are not permitted in the head of a clause at ")
             Region.pp)
    ;;

    let pp ppf = function
      | WildcardsInHead regions ->
        Fmt.(vbox @@ list ~sep:cut pp_wildcard) ppf regions
      | RangeViolations vs -> Fmt.(vbox @@ list ~sep:cut pp_violation) ppf vs
      | NoCompatibleOrder cls -> pp_no_order ppf cls
      | NegativeCycles cycles ->
        Fmt.(vbox @@ suffix (any ":@;") @@ list ~sep:cut pp_cycle) ppf cycles
    ;;

    let pp = `NoPrec pp
  end)
end

module Warn = struct
  type t = UnusedClause of Region.t

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf = function
      | UnusedClause region ->
        Fmt.(suffix (any ": unused clause") Region.pp) ppf region
    ;;

    let pp = `NoPrec pp
  end)
end

module Topic = struct
  module X = struct
    type t = Warn.t list

    let mempty = []
    let append x y = x @ y
    let pp ppf xs = Fmt.(vbox @@ list ~sep:cut Warn.pp) ppf xs
    let pp = `NoPrec pp
  end

  include X
  include Monoid.Make0 (X)
  include Pretty.Make0 (X)
end

module Env = struct
  type t =
    { reserved_names : Name.t list
    ; trg : Ty.TRG.t
    ; guard_prefix : string
    }

  let default =
    { reserved_names = []; guard_prefix = "guard"; trg = Ty.TRG.empty }
  ;;
end

module State = struct
  type t =
    { fresh_src : int
    ; typing_env : TypingEnv.t
    }

  let default = { fresh_src = 0; typing_env = TypingEnv.empty }
end

include Effect.MonadRWSError.Make (Err) (Topic) (State) (Env)

(* -- Helpers --------------------------------------------------------------- *)

let run ?(env = Env.default) ?(st = State.default) t = run ~env ~st t

let eval ?env ?st t =
  let res, _, _ = run ?env ?st t in
  res
;;

let ( let* ) x f = bind x ~f
let warn w = tell [ w ]
let warns ws = tell ws

let incr =
  gets (fun State.{ fresh_src; _ } -> fresh_src)
  >>= fun ctr ->
  modify (fun st -> { st with fresh_src = ctr + 1 }) >>= fun _ -> return ctr
;;

(* -- typing helpers -------------------------------------------------------- *)
let get_typing_env = gets (fun State.{ typing_env; _ } -> typing_env)
let set_typing_env typing_env = modify (fun st -> { st with typing_env })

let get_param_type name =
  get_typing_env >>= fun env -> return @@ TypingEnv.find_param env ~name
;;

let get_data_ttc name =
  get_typing_env >>= fun env -> return @@ TypingEnv.find_data env ~name
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

let get_pred_typing name =
  map
    ~f:
      Option.(
        value_map
          ~default:Typing.bottom
          ~f:TypingEnv.(fun { typing; _ } -> typing))
  @@ get_pred_info name
;;

let set_pred_typing name typing =
  get_typing_env
  >>= fun env ->
  set_typing_env @@ TypingEnv.update_pred_typing_exn env ~name ~typing
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
