open Core_kernel
open Core

let pred_a = Pred.(pred ~arity:1 @@ Name.from_string "a")
let pred_b = Pred.(pred ~arity:1 @@ Name.from_string "b")
let pred_c = Pred.(pred ~arity:1 @@ Name.from_string "c")
let pred_d = Pred.(pred ~arity:1 @@ Name.from_string "d")
let pred_p = Pred.(pred ~arity:1 @@ Name.from_string "p")
let pred_q = Pred.(pred ~arity:1 @@ Name.from_string "q")
let pred_r = Pred.(pred ~arity:2 @@ Name.from_string "r")
let pred_s = Pred.(pred ~arity:1 @@ Name.from_string "s")
let pred_w = Pred.(pred ~arity:1 @@ Name.from_string "w")
let pred_qry = Pred.(pred ~arity:0 @@ Name.from_string "query")

let cl_s =
  Clause.Raw.clause
    Lit.Raw.(lit pred_s Term.[ var "X" ])
    Lit.Raw.[ lit pred_a Term.[ var "X" ] ]
;;

let cl_p =
  Clause.Raw.clause
    Lit.Raw.(lit pred_p Term.[ var "X" ])
    Lit.Raw.[ lit pred_s Term.[ var "X" ] ]
;;

let cl_q =
  Clause.Raw.clause
    Lit.Raw.(lit pred_p Term.[ var "X" ])
    Lit.Raw.[ lit pred_s Term.[ var "X" ] ]
;;

let cl_qry =
  Clause.Raw.clause
    Lit.Raw.(lit pred_qry [])
    Lit.Raw.[ lit pred_p Term.[ sym @@ Symbol.from_int 2 ] ]
;;

let pairs =
  let cl_list_equal = List.equal Clause.Raw.equal
  and cl_list_pp = Fmt.(hovbox @@ brackets @@ list ~sep:comma Clause.Raw.pp) in
  let equal = Tuple2.equal ~eq1:cl_list_equal ~eq2:cl_list_equal
  and pp = Fmt.(hovbox @@ pair ~sep:comma cl_list_pp cl_list_pp) in
  Alcotest.testable pp equal
;;

(** -- Program with single dead clause `q` -------------------------------------

s(X) :- a(X).
p(X) :- s(X).
q(X) :- s(X).
query() :- p(2).

----------------------------------------------------------------------------- *)

let prg_dead_clause_single =
  Program.Raw.program [ cl_s; cl_p; cl_q; cl_qry ] [ pred_qry ] [] []
;;

let dead_clause_single () =
  Alcotest.(check pairs)
    "single covering constant symbol"
    ([ cl_p; cl_s; cl_qry ], [ cl_q ])
    Dependency.Raw.(dead_clauses prg_dead_clause_single)
;;

(** -- Program with no exposed queries -------------------------------------- *)

let prg_no_query = Program.Raw.{ prg_dead_clause_single with queries = [] }

let no_query () =
  Alcotest.(check pairs)
    "no exposed queries"
    ([], [ cl_p; cl_q; cl_s; cl_qry ])
    Dependency.Raw.(dead_clauses prg_no_query)
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

let cls =
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
;;

let prg_deeply_nested = Program.Raw.program cls [ pred_qry ] [] []

let deeply_nested () =
  Alcotest.(check pairs)
    "deeply nested, no dead clauses"
    (cls, [])
    Dependency.Raw.(dead_clauses prg_deeply_nested)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Single dead clause" `Quick dead_clause_single
    ; test_case "No exposed queries" `Quick no_query
    ; test_case "No dead clauses, deeply nested" `Quick deeply_nested
    ]
;;
