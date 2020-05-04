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

let test =
  "\n\
   edge(A,B).\n\
   edge(B,C).\n\
   edge(A,D).\n\
   edge(B,D).\n\n\
   connected(?X,?Y) \n\
  \  :- edge(?X,?Y) \n\
  \  ;  edge(?X,?Z) , connected(?Z,?Y). \n\n\
  \  complement(?X,?Y) :- !connected(?X,?Y)."
;;

(* -- Transformations ------------------------------------------------------- *)

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
