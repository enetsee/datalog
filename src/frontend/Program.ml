open Core_kernel
open Reporting
open Lib

module X = struct
  type t = { stmts : Statement.t list } [@@deriving eq, compare]

  let pp ppf { stmts } = Fmt.(vbox @@ list ~sep:cut Statement.pp) ppf stmts
  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

(* -- Query ----------------------------------------------------------------- *)

let atoms { stmts } = List.concat_map ~f:Statement.atoms stmts
let tmvars { stmts } = List.concat_map ~f:Statement.tmvars stmts

(* == Transformations ======================================================= *)

(* -- Transformation helpers ------------------------------------------------ *)

let transform_atom { stmts } ~f =
  { stmts = List.map ~f:Statement.(transform_atom ~f) stmts }
;;

let transform_head { stmts } ~f =
  { stmts = List.map ~f:Statement.(transform_head ~f) stmts }
;;

let transform_body { stmts } ~f =
  { stmts = List.map ~f:Statement.(transform_body ~f) stmts }
;;

let transform_clause { stmts } ~f =
  { stmts = List.map ~f:Statement.(transform_clause ~f) stmts }
;;

let transform_query { stmts } ~f =
  { stmts = List.map ~f:Statement.(transform_query ~f) stmts }
;;

let transform_fact { stmts } ~f =
  { stmts = List.map ~f:Statement.(transform_fact ~f) stmts }
;;

let transform_sentence { stmts } ~f =
  { stmts = List.map ~f:Statement.(transform_sentence ~f) stmts }
;;

let transform_statement { stmts } ~f = { stmts = List.map ~f stmts }

module Effect (M : sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t
end) =
struct
  module S = Statement.Effect (M)

  let transform_atom { stmts } ~f =
    M.(
      List.map ~f:S.(transform_atom ~f) stmts
      |> all
      |> map ~f:(fun stmts -> { stmts }))
  ;;

  let transform_head { stmts } ~f =
    M.(
      List.map ~f:S.(transform_head ~f) stmts
      |> all
      |> map ~f:(fun stmts -> { stmts }))
  ;;

  let transform_body { stmts } ~f =
    M.(
      List.map ~f:S.(transform_body ~f) stmts
      |> all
      |> map ~f:(fun stmts -> { stmts }))
  ;;

  let transform_clause { stmts } ~f =
    M.(
      List.map ~f:S.(transform_clause ~f) stmts
      |> all
      |> map ~f:(fun stmts -> { stmts }))
  ;;

  let transform_query { stmts } ~f =
    M.(
      List.map ~f:S.(transform_query ~f) stmts
      |> all
      |> map ~f:(fun stmts -> { stmts }))
  ;;

  let transform_fact { stmts } ~f =
    M.(
      List.map ~f:S.(transform_fact ~f) stmts
      |> all
      |> map ~f:(fun stmts -> { stmts }))
  ;;

  let transform_sentence { stmts } ~f =
    M.(
      List.map ~f:S.(transform_sentence ~f) stmts
      |> all
      |> map ~f:(fun stmts -> { stmts }))
  ;;

  let transform_statement { stmts } ~f =
    M.(List.map ~f stmts |> all |> map ~f:(fun stmts -> { stmts }))
  ;;
end

module Logged = Effect (Logger)

(* -- Normalization and preprocessing --------------------------------------- *)
let set_foreign_func prog ~ffns =
  Logged.transform_sentence ~f:Sentence.(set_foreign_func ~ffns) prog
;;

let name_query prog = Logged.transform_sentence ~f:Sentence.name_query prog
let push_neg prog = transform_body ~f:Subgoal.push_neg prog
let collect_disj prog = transform_body ~f:Subgoal.collect_disj prog

let split_disj { stmts } =
  Logger.(
    List.map ~f:Statement.split_disj stmts
    |> all
    >>= fun stmtss -> return { stmts = List.concat stmtss })
;;

let normalize prog = split_disj @@ collect_disj @@ push_neg prog

(* -- Translate to `Core` language ------------------------------------------ *)

let clauses_of { stmts } =
  List.filter_map
    stmts
    ~f:
      Option.(
        fun st ->
          Statement.lower_sentence st
          >>= Sentence.lower_clause
          |> map ~f:Located.elem_of)
;;

let queries_of { stmts } =
  List.filter_map
    stmts
    ~f:
      Option.(
        fun st ->
          Statement.lower_sentence st
          >>= Sentence.lower_query
          |> map ~f:Located.elem_of)
;;

let facts_of { stmts } =
  List.filter_map
    stmts
    ~f:
      Option.(
        fun st ->
          Statement.lower_sentence st
          >>= Sentence.lower_fact
          |> map ~f:Located.elem_of)
;;

let query_preds qs =
  List.map ~f:(fun Core.Clause.{ head; _ } -> head.pred) qs
  |> List.dedup_and_sort ~compare:Core.Pred.(compare (fun _ _ -> 0))
;;

let to_core prog =
  Logger.(
    facts_of prog
    |> List.map ~f:Fact.to_core
    |> all
    >>= fun edb ->
    clauses_of prog
    |> List.map ~f:Clause.to_core
    |> all
    >>= fun clauses ->
    queries_of prog
    |> List.map ~f:Query.to_core
    |> all
    >>= fun queries ->
    return
      Core.Program.
        { strata = [ Core.Stratum.from_list @@ clauses @ queries ]
        ; queries = query_preds queries
        })
;;
