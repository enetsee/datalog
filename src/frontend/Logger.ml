open Core_kernel
include Core.Logger

let rec fresh () =
  Core.Logger.(
    get
    >>= fun (State.{ counter; reserved; prefix } as st) ->
    put { st with counter = counter + 1 }
    >>= fun _ ->
    let ctr_string = string_of_int counter in
    let candidate =
      Option.value_map
        ~default:ctr_string
        ~f:(fun pfx -> pfx ^ ctr_string)
        prefix
    in
    if String.Set.mem reserved candidate then fresh () else return candidate)
;;

(* -- Warning --------------------------------------------------------------- *)

let warn warning =
  Core.Logger.(
    get
    >>= fun (State.{ warnings; _ } as st) ->
    put { st with warnings = warning :: warnings })
;;

let predsym_clash_with_ffn region = Warning.PredSymClash region

(* -- Failure --------------------------------------------------------------- *)
let fail err = Core.Logger.lift @@ Error err
let parse_error msg region = Err.Parse (msg, region)
let parse_bad_state = Err.ParseBadState
let query_already_named region = Err.QueryNamed region
let query_not_named region = Err.QueryUnnamed region
let fact_not_range_restricted region = Err.FactNotRangeRestricted region
let fact_has_wildcard region = Err.FactHasWildcard region
let head_not_atom region = Err.HeadNotAtom region

let ffn_wrong_arity region ~actual ~expect =
  Err.FfnWrongArity (region, actual, expect)
;;

let clause_negation region = Err.ClauseNegation region
let clause_disjunction region = Err.ClauseDisjunction region
let clause_binop region = Err.ClauseBinOp region
let clause_unop region = Err.ClauseUnOp region
let clause_nullop = Err.ClauseNullOp
