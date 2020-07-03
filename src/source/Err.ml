open Lib
open Reporting

type t =
  | QueryNamed of Region.t
  | QueryUnnamed of Region.t
  | FactNotRangeRestricted of Region.t
  | FactHasWildcard of Region.t
  | HeadNotAtom of Region.t
  | ClauseBinOp of Region.t
  | ClauseUnOp of Region.t
  | ClauseNullOp
  | ClauseNeg of Region.t
  | ClauseDisj of Region.t

let query_already_named region = QueryNamed region
let query_not_named region = QueryUnnamed region
let fact_not_range_restricted region = FactNotRangeRestricted region
let fact_has_wildcard region = FactHasWildcard region
let head_not_atom region = HeadNotAtom region
let clause_binop region = ClauseBinOp region
let clause_unop region = ClauseUnOp region
let clause_nullop = ClauseNullOp

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
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
      Fmt.(any "Facts may not contain wildcards " ++ parens Region.pp)
        ppf
        region
    | HeadNotAtom region ->
      Fmt.(any "Head is not an atom " ++ parens Region.pp) ppf region
    | ClauseNullOp -> Fmt.(string ppf "Nullary operators cannot be compiled ")
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
    | ClauseNeg region ->
      Fmt.(
        any
          "Negation can only be applied to atoms in core. Clause is not in \
           normal form."
        ++ parens Region.pp)
        ppf
        region
    | ClauseDisj region ->
      Fmt.(
        any "Disjunction is not allowed in core. Clause is not in normal form."
        ++ parens Region.pp)
        ppf
        region
  ;;

  let pp = `NoPrec pp
end)
