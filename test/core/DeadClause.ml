open Core

let pred_a = Pred.(logical ~arity:1 @@ Name.from_string "a")
let pred_b = Pred.(logical ~arity:1 @@ Name.from_string "b")
let pred_c = Pred.(logical ~arity:1 @@ Name.from_string "c")
let pred_d = Pred.(logical ~arity:1 @@ Name.from_string "d")
let pred_p = Pred.(logical ~arity:1 @@ Name.from_string "p")
let pred_q = Pred.(logical ~arity:1 @@ Name.from_string "q")
let pred_r = Pred.(logical ~arity:2 @@ Name.from_string "r")
let pred_s = Pred.(logical ~arity:1 @@ Name.from_string "s")
let pred_w = Pred.(logical ~arity:1 @@ Name.from_string "w")
let pred_qry = Pred.(logical ~arity:0 @@ Name.from_string "query")
let testable_prg = Program.Raw.(Alcotest.testable pp equal)

(** -- Program with single dead clause `q` -------------------------------------

s(X) :- a(X).
p(X) :- s(X).
q(X) :- s(X).
query() :- p(2).

----------------------------------------------------------------------------- *)

let prg_dead_clause_single =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_s Term.[ var "X" ])
          Lit.Raw.[ lit pred_a Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_s Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_q Term.[ var "X" ])
          Lit.Raw.[ lit pred_s Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_p Term.[ sym @@ Symbol.from_int 2 ] ]
      ]
    [ pred_qry ]
;;

let prg_dead_clause_single_expected =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_s Term.[ var "X" ])
          Lit.Raw.[ lit pred_a Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_s Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_p Term.[ sym @@ Symbol.from_int 2 ] ]
      ]
    [ pred_qry ]
;;

let dead_clause_single () =
  Alcotest.check
    testable_prg
    "single covering constant symbol"
    prg_dead_clause_single_expected
    Compile.(elim_dead_clauses prg_dead_clause_single)
;;

(** -- Program with no exposed queries -------------------------------------- *)

let prg_no_query = Program.Raw.{ prg_dead_clause_single with queries = [] }
let prg_empty = Program.Raw.program [] []

let no_query () =
  Alcotest.check
    testable_prg
    "no exposed queries"
    prg_empty
    Compile.(elim_dead_clauses prg_no_query)
;;

(** -- Deeply nested ----------------------------------------------------------


b(X) :- a(X).
c(X) :- b(X).
d(X) :- c(X).
p(X) :- d(X).
q(X) :- p(X).
s(X) :- q(X).
query() :- w(X), s(X).

*)

let prg_deeply_nested =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_b Term.[ var "X" ])
          Lit.Raw.[ lit pred_a Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_c Term.[ var "X" ])
          Lit.Raw.[ lit pred_b Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_d Term.[ var "X" ])
          Lit.Raw.[ lit pred_c Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_d Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_q Term.[ var "X" ])
          Lit.Raw.[ lit pred_p Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_s Term.[ var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_w Term.[ var "X" ]; lit pred_s Term.[ var "X" ] ]
      ]
    [ pred_qry ]
;;

let deeply_nested () =
  Alcotest.check
    testable_prg
    "deeply nested, no dead clauses"
    prg_deeply_nested
    Compile.(elim_dead_clauses prg_deeply_nested)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Single dead clause" `Quick dead_clause_single
    ; test_case "No exposed queries" `Quick no_query
    ; test_case "No dead clauses, deeply nested" `Quick deeply_nested
    ]
;;
