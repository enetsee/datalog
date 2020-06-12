open Core_kernel
open Lib
open Reporting

module Err = struct
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
          any "Erroneous binary operator encounted in clause "
          ++ parens Region.pp)
          ppf
          region
      | ClauseUnOp region ->
        Fmt.(
          any "Erroneous unary operator encounted in clause "
          ++ parens Region.pp)
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
          any
            "Disjunction is not allowed in core. Clause is not in normal form."
          ++ parens Region.pp)
          ppf
          region
    ;;

    let pp = `NoPrec pp
  end)
end

module Warn = struct
  type t =
    | PredNameClash of Region.t
    | NoQueries
    | Undeclared of Region.t

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf = function
      | PredNameClash region ->
        Fmt.(
          any "Predicate symbol clashes with a reserved name "
          ++ parens Region.pp)
          ppf
          region
      | Undeclared region ->
        Fmt.(any "Predicate symbol has not been declared " ++ parens Region.pp)
          ppf
          region
      | NoQueries -> Fmt.string ppf "The program does not expose any queries"
    ;;

    let pp = `NoPrec pp
  end)
end

module Env = struct
  type t =
    { extras : Core.Nature.t Core.Pred.Name.Map.t
    ; cnstrts : Core.Constraint.t Core.Pred.Map.t
    ; reserved_names : Core.Pred.Name.t list
    ; query_prefix : string
    }

  let default =
    { extras = Core.Pred.Name.Map.empty
    ; cnstrts = Core.Pred.Map.empty
    ; reserved_names = []
    ; query_prefix = "query"
    }
  ;;
end

module State = struct
  type t = { fresh_src : int }

  let default = { fresh_src = 0 }
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

include Effect.MonadRWSError.Make (Err) (Topic) (State) (Env)

let run ?(env = Env.default) ?(st = State.default) t = run ~env ~st t
let warn w = tell [ w ]

let incr =
  gets (fun State.{ fresh_src; _ } -> fresh_src)
  >>= fun ctr ->
  modify (fun _st -> { fresh_src = ctr + 1 }) >>= fun _ -> return ctr
;;

let query_prefix = map ~f:(fun Env.{ query_prefix; _ } -> query_prefix) ask

let fresh_predsym pfx =
  incr >>= fun i -> return Core.Pred.Name.(from_string @@ pfx ^ string_of_int i)
;;

let fresh_querysym = query_prefix >>= fresh_predsym
let natures = reader (fun { extras; _ } -> extras)
let constraints = reader (fun { cnstrts; _ } -> cnstrts)
