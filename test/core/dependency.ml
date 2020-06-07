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

let dep_dead_clause_single = Dependency.from_program prg_dead_clause_single
