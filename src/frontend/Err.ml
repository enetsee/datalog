open Lib
open Reporting

type t =
  | Parse of string option * Region.t
  | ParseBadState
  | QueryNamed of Region.t
  | QueryUnnamed of Region.t
  | FactNotRangeRestricted of Region.t
  | FactHasWildcard of Region.t
  | HeadNotAtom of Region.t
  | FfnWrongArity of Region.t * int * int
  | ClauseNegation of Region.t
  | ClauseDisjunction of Region.t
  | ClauseBinOp of Region.t
  | ClauseUnOp of Region.t
  | ClauseNullOp

let pp ppf = function
  | Parse (Some msg, region) ->
    Fmt.(pair ~sep:sp (any "Parse error: " ++ string) (parens Region.pp))
      ppf
      (msg, region)
  | Parse (_, region) ->
    Fmt.(any "Parse error without message " ++ Region.pp) ppf region
  | ParseBadState -> Fmt.string ppf "Parsing failed in an error state"
  | QueryNamed region ->
    Fmt.(any "Query already named " ++ parens Region.pp) ppf region
  | QueryUnnamed region ->
    Fmt.(any "Query not named " ++ parens Region.pp) ppf region
  | FactNotRangeRestricted region ->
    Fmt.(
      any "Fact contains a variable which violates range restriction "
      ++ parens Region.pp)
      ppf
      region
  | FactHasWildcard region ->
    Fmt.(any "Facts may not contain wildcards " ++ parens Region.pp) ppf region
  | HeadNotAtom region ->
    Fmt.(any "Head is not an atom " ++ parens Region.pp) ppf region
  | FfnWrongArity (region, expected, actual) ->
    Fmt.pf
      ppf
      {|Function expects %i arguments but %i were provided %a|}
      expected
      actual
      Fmt.(parens Region.pp)
      region
  | ClauseNullOp -> Fmt.(string ppf "Nullary operators cannot be compiled ")
  | ClauseNegation region ->
    Fmt.(
      any "Negation applied to non-atomic formula in clause "
      ++ parens Region.pp)
      ppf
      region
  | ClauseDisjunction region ->
    Fmt.(any "Disjunction not eliminated in clause " ++ parens Region.pp)
      ppf
      region
  | ClauseBinOp region ->
    Fmt.(
      any "Erroneous binary operator encounted in clause " ++ parens Region.pp)
      ppf
      region
  | ClauseUnOp region ->
    Fmt.(
      any "Erroneous unary operator encounted in clause " ++ parens Region.pp)
      ppf
      region
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp = `NoPrec pp
end)
