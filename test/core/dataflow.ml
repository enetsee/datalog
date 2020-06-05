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
let testable_src = Dataflow.Src.(Alcotest.testable pp equal)

(** -- Covering constant -------------------------------------------------------

p(X) :- q(X).
query() :- p(1).
 
covering literal of q(X)@0 is constant 1
*)

let prg_const =
  Program.program
    Clause.
      [ clause
          Lit.(lit pred_p Term.[ var "X" ])
          Lit.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.(lit pred_qry [])
          Lit.[ lit pred_p Term.[ sym @@ Symbol.from_int 1 ] ]
      ]
    [ pred_qry ]
;;

let covering_const_expect =
  Some Dataflow.[ Src.SConst (Const.CSym (Symbol.from_int 1)) ]
;;

let covering_constant () =
  Alcotest.(check @@ option @@ list testable_src)
    "single covering constant symbol"
    covering_const_expect
    Dataflow.(coveringPositives ~dest:destQ @@ from_prog prg_const)
;;

(** -- Covering constant with additional dead path -----------------------------

p(X) :- q(X).
query() :- p(1).
s(X) :- p(X).

covering literal of q(X)@0 is constant 1
*)
let prg_dead_path =
  Program.program
    Clause.
      [ clause
          Lit.(lit pred_p Term.[ var "X" ])
          Lit.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.(lit pred_qry [])
          Lit.[ lit pred_p Term.[ sym @@ Symbol.from_int 1 ] ]
      ; clause
          Lit.(lit pred_s Term.[ var "X" ])
          Lit.[ lit pred_p Term.[ var "X" ] ]
      ]
    [ pred_qry ]
;;

let covering_dead_path () =
  Alcotest.(check @@ option @@ list testable_src)
    "single covering constant with dead path"
    covering_const_expect
    Dataflow.(coveringPositives ~dest:destQ @@ from_prog prg_dead_path)
;;

(** -- Covering constant but predicate `p` also exposed as a query -------------

p(X) :- q(X).
query() :- p(1).
 
no covering constants
*)

let prg_const_exposed =
  Program.{ prg_const with queries = pred_p :: prg_const.queries }
;;

let covering_const_exposed () =
  Alcotest.(check @@ option @@ list testable_src)
    "single covering constant with dead path"
    None
    Dataflow.(coveringPositives ~dest:destQ @@ from_prog prg_const_exposed)
;;

(** -- Covering wildcard -------------------------------------------------------

p(X) :- q(X).
query() :- p(_).
 
covering literal of q(X)@0 is constant _
*)

let prg_wild =
  Program.program
    Clause.
      [ clause
          Lit.(lit pred_p Term.[ var "X" ])
          Lit.[ lit pred_q Term.[ var "X" ] ]
      ; clause Lit.(lit pred_qry []) Lit.[ lit pred_p Term.[ wild () ] ]
      ]
    [ pred_qry ]
;;

let covering_wild () =
  Alcotest.(check @@ option @@ list testable_src)
    "single covering constant wildcard"
    (Some Dataflow.[ Src.SConst Const.CWild ])
    Dataflow.(coveringPositives ~dest:destQ @@ from_prog prg_wild)
;;

let test_cases =
  Alcotest.
    [ test_case "Covering constant symbol" `Quick covering_constant
    ; test_case
        "Covering constant with additional dead path"
        `Quick
        covering_dead_path
    ; test_case
        "Covering constant but predicate exposed"
        `Quick
        covering_const_exposed
    ; test_case "Covering constant wildcard" `Quick covering_wild
    ]
;;
