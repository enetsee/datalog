open Core_kernel
open Lib
open Reporting

module Minimal = struct
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
        | ClauseNullOp ->
          Fmt.(string ppf "Nullary operators cannot be compiled ")
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
              "Disjunction is not allowed in core. Clause is not in normal \
               form."
            ++ parens Region.pp)
            ppf
            region
      ;;

      let pp = `NoPrec pp
    end)
  end

  module Warn = struct
    type t = PredNameClash of Region.t

    include Pretty.Make0 (struct
      type nonrec t = t

      let pp ppf = function
        | PredNameClash region ->
          Fmt.(
            any "Predicate symbol clashes with a reserved name "
            ++ parens Region.pp)
            ppf
            region
      ;;

      let pp = `NoPrec pp
    end)
  end

  module Env = struct
    type t =
      { extras : Core.Nature.t Core.PredSymbol.Map.t
      ; cnstrts : Core.Constraint.t Core.PredSymbol.Map.t
      ; reserved_names : Core.PredSymbol.t
      ; query_prefix : string
      }
  end

  module State = struct
    type t = { fresh_src : int }
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

  type 'a t =
    { apply :
        'o. Env.t -> State.t
        -> k:(('a, Err.t) result -> Topic.t -> State.t -> 'o) -> 'o
    }

  let run { apply } ~env ~st = apply env st ~k:(fun r w s -> r, w, s)

  let map { apply = g } ~f =
    { apply = (fun env st ~k -> g env st ~k:(fun a -> k @@ Result.map ~f a)) }
  ;;

  let map = `Custom map
  let return a = { apply = (fun _ st ~k -> k (Ok a) Topic.mempty st) }

  let bind { apply = g } ~f =
    { apply =
        (fun env st ~k ->
          g env st ~k:(fun a_res w st' ->
              match Result.map ~f a_res with
              | Error err -> k (Error err) w st'
              | Ok { apply } ->
                apply env st' ~k:(fun b_res w' st'' ->
                    k b_res Topic.(append w w') st'')))
    }
  ;;

  let apply mf mt = bind mf ~f:(fun f -> bind mt ~f:(fun t -> return @@ f t))
end

module Effects = struct
  include Minimal
  include Monad.Make (Minimal)
  include Applicative.Make (Minimal)

  let writer (a, warn) = { apply = (fun _ st ~k -> k (Ok a) warn st) }

  let listen { apply = f } =
    { apply =
        (fun env st ~k ->
          f env st ~k:(fun a_res w st' ->
              k Result.(map ~f:(fun a -> a, w) a_res) w st'))
    }
  ;;

  let pass { apply = f } =
    { apply =
        (fun env st ~k ->
          f env st ~k:(fun a_res w st' ->
              match a_res with
              | Error err -> k (Error err) w st'
              | Ok (a, p) -> k (Ok a) (p w) st'))
    }
  ;;

  let reader f = { apply = (fun env st ~k -> k (Ok (f env)) Topic.mempty st) }
  let local { apply = g } ~f = { apply = (fun env st ~k -> g (f env) st ~k) }

  let state f =
    { apply =
        (fun _ st ~k ->
          let a, st' = f st in
          k (Ok a) Topic.mempty st')
    }
  ;;

  let fail err = { apply = (fun _ st ~k -> k (Error err) Topic.mempty st) }
end

include Effects
include Effect.MonadWriter.Make (Effects)
include Effect.MonadReader.Make (Effects)
include Effect.MonadState.Make (Effects)

let warn w = tell [ w ]

let incr =
  gets (fun State.{ fresh_src; _ } -> fresh_src)
  >>= fun ctr ->
  modify (fun _st -> { fresh_src = ctr + 1 }) >>= fun _ -> return ctr
;;

let query_prefix = map ~f:(fun Env.{ query_prefix; _ } -> query_prefix) ask

let fresh_predsym pfx =
  incr
  >>= fun i -> return Core.PredSymbol.(from_string @@ pfx ^ string_of_int i)
;;

let fresh_querysym = query_prefix >>= fresh_predsym
let natures = reader (fun { extras; _ } -> extras)
let constraints = reader (fun { cnstrts; _ } -> cnstrts)
