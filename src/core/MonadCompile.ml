open Core_kernel
open Reporting
open Lib

module Err = struct
  type t =
    | RangeWildcard of Dataflow.Dest.t * Region.t
    | RangeViolations of Violation.t list
    | NoCompatibleOrder of (Binding.t * Region.t) list
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

    let pp ppf = function
      | RangeWildcard (_, region) ->
        Fmt.(hovbox @@ suffix (any "") Region.pp) ppf region
      | RangeViolations vs -> Fmt.(vbox @@ list ~sep:cut pp_violation) ppf vs
      | NoCompatibleOrder cls ->
        Fmt.(vbox @@ suffix (any ":@;") @@ list ~sep:cut pp_no_order) ppf cls
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
    { reserved_names : Pred.Name.t list
    ; guard_prefix : string
    }

  let default = { reserved_names = []; guard_prefix = "guard" }
end

module State = struct
  type t = { fresh_src : int }

  let default = { fresh_src = 0 }
end

include Effect.MonadRWSError.Make (Err) (Topic) (State) (Env)

(* -- Helpers --------------------------------------------------------------- *)

let run ?(env = Env.default) ?(st = State.default) t = run ~env ~st t

let eval ?env ?st t =
  let res, _, _ = run ?env ?st t in
  res
;;

let warn w = tell [ w ]
let warns ws = tell ws

let incr =
  gets (fun State.{ fresh_src; _ } -> fresh_src)
  >>= fun ctr ->
  modify (fun _st -> { fresh_src = ctr + 1 }) >>= fun _ -> return ctr
;;

let guard_prefix = map ~f:(fun Env.{ guard_prefix; _ } -> guard_prefix) ask

let fresh_predsym pfx =
  incr >>= fun i -> return Pred.Name.(from_string @@ pfx ^ string_of_int i)
;;

let fresh_guardsym = guard_prefix >>= fresh_predsym
