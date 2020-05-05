open Core_kernel
open Lib

(* A wrapper around StateT Result to hide complexity and provide some helpers *)

(** TODO: flatten this down and possibly add reader? *)
module M = Mtl.StateT.Make2 (Result)

module Warning = struct
  type t = PredSymClash of Reporting.Region.t

  let pp ppf = function
    | PredSymClash region ->
      Fmt.(
        any "Predicate symbol clashes with a reserved name "
        ++ parens Reporting.Region.pp)
        ppf
        region
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)
end

module State = struct
  type t =
    { prefix : string option
    ; reserved : String.Set.t
    ; warnings : Warning.t list
    ; counter : int
    }

  let init prefix reserved = { prefix; reserved; warnings = []; counter = 0 }
end

module Err = struct
  type t =
    | Parse of string option * Reporting.Region.t
    | ParseBadState
    | QueryNamed of Reporting.Region.t
    | QueryUnnamed of Reporting.Region.t
    | FactNotRangeRestricted of Reporting.Region.t
    | FactHasWildcard of Reporting.Region.t
    | HeadNotAtom of Reporting.Region.t
    | FfnWrongArity of Reporting.Region.t * int * int
    | NullOpNotTranslatable of Reporting.Region.t

  let pp ppf = function
    | Parse (Some msg, region) ->
      Fmt.(
        pair
          ~sep:sp
          (any "Parse error: " ++ string)
          (parens Reporting.Region.pp))
        ppf
        (msg, region)
    | Parse (_, region) ->
      Fmt.(any "Parse error without message " ++ Reporting.Region.pp) ppf region
    | ParseBadState -> Fmt.string ppf "Parsing failed in an error state"
    | QueryNamed region ->
      Fmt.(any "Query already named " ++ parens Reporting.Region.pp) ppf region
    | QueryUnnamed region ->
      Fmt.(any "Query not named " ++ parens Reporting.Region.pp) ppf region
    | FactNotRangeRestricted region ->
      Fmt.(
        any "Fact contains a variable which violates range restriction "
        ++ parens Reporting.Region.pp)
        ppf
        region
    | FactHasWildcard region ->
      Fmt.(any "Facts may not contain wildcards " ++ parens Reporting.Region.pp)
        ppf
        region
    | HeadNotAtom region ->
      Fmt.(any "Head is not an atom " ++ parens Reporting.Region.pp) ppf region
    | FfnWrongArity (region, expected, actual) ->
      Fmt.pf
        ppf
        {|Function expects %i arguments but %i were provided %a|}
        expected
        actual
        Fmt.(parens Reporting.Region.pp)
        region
    | NullOpNotTranslatable region ->
      Fmt.(
        any "Nullary operators cannot be compiled "
        ++ parens Reporting.Region.pp)
        ppf
        region
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)
end

module Minimal = struct
  type 'a t = ('a, State.t, Err.t) M.t

  let bind = M.bind
  let map = `Custom M.map
  let apply = M.apply
  let return = M.return
end

include Minimal
include Monad.Make (Minimal)
include Applicative.Make (Minimal)

let run ?(prefix = None) reserved t =
  M.run t ~init:State.(init prefix reserved)
  |> Result.map ~f:(fun (res, State.{ warnings; _ }) -> res, warnings)
;;

let rec fresh () : string t =
  M.(
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

(* -- Failure --------------------------------------------------------------- *)
let fail err = M.lift @@ Error err
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

(* -- Warning --------------------------------------------------------------- *)

let warn warning : unit t =
  M.(
    get
    >>= fun (State.{ warnings; _ } as st) ->
    put { st with warnings = warning :: warnings })
;;

let predsym_clash_with_ffn region = Warning.PredSymClash region
