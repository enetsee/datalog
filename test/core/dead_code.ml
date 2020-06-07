open Core
open Raw

let pred_a = Pred.(logical ~arity:1 @@ Name.from_string "a")
let pred_p = Pred.(logical ~arity:1 @@ Name.from_string "p")
let pred_q = Pred.(logical ~arity:1 @@ Name.from_string "q")
let pred_r = Pred.(logical ~arity:2 @@ Name.from_string "r")
let pred_s = Pred.(logical ~arity:1 @@ Name.from_string "s")
let pred_qry = Pred.(logical ~arity:0 @@ Name.from_string "query")
let pred_qry2 = Pred.(logical ~arity:0 @@ Name.from_string "query2")
let destQ = Dataflow.Dest.DLit (Lit.lit pred_q Term.[ var "X" ], 0)
let destP = Dataflow.Dest.DPred (pred_p, 0)
let destR = Dataflow.Dest.DLit (Lit.lit pred_r Term.[ var "X"; var "X" ], 0)
let testable_prg = Program.(Alcotest.testable pp equal)

(** -- Program with single dead clause `q` -------------------------------------

s(X) :- a(X).
p(X) :- s(X).
q(X) :- s(X).
query() :- p(2).

----------------------------------------------------------------------------- *)

let prg_dead_clause_single =
  Program.program
    Clause.
      [ clause
          Lit.(lit pred_s Term.[ var "X" ])
          Lit.[ lit pred_a Term.[ var "X" ] ]
      ; clause
          Lit.(lit pred_p Term.[ var "X" ])
          Lit.[ lit pred_s Term.[ var "X" ] ]
      ; clause
          Lit.(lit pred_q Term.[ var "X" ])
          Lit.[ lit pred_s Term.[ var "X" ] ]
      ; clause
          Lit.(lit pred_qry [])
          Lit.[ lit pred_p Term.[ sym @@ Symbol.from_int 2 ] ]
      ]
    [ pred_qry ]
;;

let prg_dead_clause_single_expected =
  Program.program
    Clause.
      [ clause
          Lit.(lit pred_s Term.[ var "X" ])
          Lit.[ lit pred_a Term.[ var "X" ] ]
      ; clause
          Lit.(lit pred_p Term.[ var "X" ])
          Lit.[ lit pred_s Term.[ var "X" ] ]
      ; clause
          Lit.(lit pred_qry [])
          Lit.[ lit pred_p Term.[ sym @@ Symbol.from_int 2 ] ]
      ]
    [ pred_qry ]
;;

let dead_clause_single () =
  Alcotest.check
    testable_prg
    "single covering constant symbol"
    prg_dead_clause_single_expected
    DeadClause.(apply prg_dead_clause_single)
;;

(** -- Program with no exposed queries -----------------------------------------

s(X) :- a(X).
p(X) :- s(X).
q(X) :- s(X).
query() :- p(2).

----------------------------------------------------------------------------- *)

let prg_no_query = Program.{ prg_dead_clause_single with queries = [] }
let prg_empty = Program.program [] []

let no_query () =
  Alcotest.check
    testable_prg
    "no exposed queries"
    prg_empty
    DeadClause.(apply prg_no_query)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Single dead clause" `Quick dead_clause_single
    ; test_case "No exposed queries" `Quick no_query
    ]
;;
