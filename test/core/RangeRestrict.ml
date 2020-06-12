(** The range restriction violation check and repair assumes that all 
    dead clauses have already been removed from the program 
*)
open Core_kernel

open Core

let pred_p = Pred.(logical ~arity:1 @@ Name.from_string "p")
let pred_q = Pred.(logical ~arity:0 @@ Name.from_string "q")
let pred_r = Pred.(logical ~arity:1 @@ Name.from_string "r")
let pred_s = Pred.(logical ~arity:1 @@ Name.from_string "s")
let pred_qry = Pred.(logical ~arity:0 @@ Name.from_string "query")
let pred_qry2 = Pred.(logical ~arity:0 @@ Name.from_string "query2")
let i = ref 0
let reset () = i := 0

let fresh_pred_sym pfx =
  let sym = pfx ^ string_of_int !i in
  i := !i + 1;
  Pred.Name.from_string sym
;;

let mk_guard () = Pred.(logical ~arity:1 @@ fresh_pred_sym "guard")

let output =
  Alcotest.(
    Testable.(result (tuple2 (module Program.Raw) (module Knowledge.Base)) err))
;;

(** -- Fixable with guard ------------------------------------------------------
  #
p(X) :- q().      
  ^
  +-----------------+
                    |
query() :- r(X), p(X).
             |     ^
             +-----+
      
p(X)@0 is a range violation but is reachable only via `query` clause where `X`
is range restricted.
----------------------------------------------------------------------------- *)

let prg_fixable_guard =
  Program.Raw.program
    Clause.Raw.
      [ clause Lit.Raw.(lit pred_p Term.[ var "X" ]) Lit.Raw.[ lit pred_q [] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_r Term.[ var "X" ]; lit pred_p Term.[ var "X" ] ]
      ]
    [ pred_qry ]
;;

let fixable_guard_expected =
  reset ();
  let pred_grd = mk_guard () in
  ( Program.Raw.program
      Clause.Raw.
        [ clause
            Lit.Raw.(lit pred_p Term.[ var "X" ])
            Lit.Raw.[ lit pred_grd Term.[ var "X" ]; lit pred_q [] ]
        ; clause
            Lit.Raw.(lit pred_qry [])
            Lit.Raw.[ lit pred_r Term.[ var "X" ]; lit pred_p Term.[ var "X" ] ]
        ; clause
            Lit.Raw.(lit pred_grd Term.[ var "X" ])
            Lit.Raw.[ lit pred_r Term.[ var "X" ] ]
        ]
      [ pred_qry ]
  , Knowledge.Base.empty )
;;

let fixable_guard () =
  Alcotest.(check output)
    "Fixable with guard"
    (Ok fixable_guard_expected)
    (MonadCompile.eval RangeRepair.(apply prg_fixable_guard))
;;

(** -- Fixable with knowledge --------------------------------------------------
  #
p(X) :- q().      
  ^
  +----------+
             |
query() :- p(1).
           

----------------------------------------------------------------------------- *)

let prg_fixable_knowledge =
  Program.Raw.program
    Clause.Raw.
      [ clause Lit.Raw.(lit pred_p Term.[ var "X" ]) Lit.Raw.[ lit pred_q [] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_p Term.[ sym @@ Symbol.from_int 1 ] ]
      ]
    [ pred_qry ]
;;

let fixable_knowledge_expected =
  reset ();
  let pred_grd = mk_guard () in
  ( Program.Raw.program
      Clause.Raw.
        [ clause
            Lit.Raw.(lit pred_p Term.[ var "X" ])
            Lit.Raw.[ lit pred_grd Term.[ var "X" ]; lit pred_q [] ]
        ; clause
            Lit.Raw.(lit pred_qry [])
            Lit.Raw.[ lit pred_p Term.[ sym @@ Symbol.from_int 1 ] ]
        ]
      [ pred_qry ]
  , Knowledge.(Base.singleton @@ knowledge pred_grd [ Symbol.from_int 1 ]) )
;;

let fixable_knowledge () =
  Alcotest.(check output)
    "Fixable with knowledge"
    (Ok fixable_knowledge_expected)
    (MonadCompile.eval RangeRepair.(apply prg_fixable_knowledge))
;;

(** -- Fixable, multiple paths -------------------------------------------------

----------------------------------------------------------------------------- *)
let prg_fixable_multi =
  Program.Raw.program
    Clause.Raw.
      [ clause Lit.Raw.(lit pred_p Term.[ var "X" ]) Lit.Raw.[ lit pred_q [] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_r Term.[ var "X" ]; lit pred_p Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_qry2 [])
          Lit.Raw.[ lit pred_p Term.[ sym @@ Symbol.from_int 1 ] ]
      ]
    [ pred_qry ]
;;

let fixable_multi_expected =
  reset ();
  let pred_grd = mk_guard () in
  ( Program.Raw.program
      Clause.Raw.
        [ clause
            Lit.Raw.(lit pred_p Term.[ var "X" ])
            Lit.Raw.[ lit pred_grd Term.[ var "X" ]; lit pred_q [] ]
        ; clause
            Lit.Raw.(lit pred_qry [])
            Lit.Raw.[ lit pred_r Term.[ var "X" ]; lit pred_p Term.[ var "X" ] ]
        ; clause
            Lit.Raw.(lit pred_qry2 [])
            Lit.Raw.[ lit pred_p Term.[ sym @@ Symbol.from_int 1 ] ]
        ; clause
            Lit.Raw.(lit pred_grd Term.[ var "X" ])
            Lit.Raw.[ lit pred_r Term.[ var "X" ] ]
        ]
      [ pred_qry ]
  , Knowledge.(Base.singleton @@ knowledge pred_grd [ Symbol.from_int 1 ]) )
;;

let fixable_multi () =
  Alcotest.(check output)
    "Fixable on multiple paths"
    (Ok fixable_multi_expected)
    (MonadCompile.eval RangeRepair.(apply prg_fixable_multi))
;;

(** -- Unfixable  --------------------------------------------------------------
  #
p(X) :- q().      
  
query() :- p(X), r(X).
             
      
This is currently unfixable even though  we could simply move `p(X)` in the
`query` clause to a position where `X` was bound.
----------------------------------------------------------------------------- *)

let prg_unfixable_unbound =
  Program.Raw.program
    Clause.Raw.
      [ clause Lit.Raw.(lit pred_p Term.[ var "X" ]) Lit.Raw.[ lit pred_q [] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_p Term.[ var "X" ]; lit pred_r Term.[ var "X" ] ]
      ]
    [ pred_qry ]
;;

let unfixable_unbound_expected =
  reset ();
  MonadCompile.Err.RangeViolations
    [ Violation.violation
        Dataflow.Dest.(DPred (pred_p, 0))
        Tmvar.(from_string "X")
    ]
;;

let unfixable_unbound () =
  Alcotest.(check output)
    "Unfixable, unbound variable"
    (Error unfixable_unbound_expected)
    (MonadCompile.eval RangeRepair.(apply prg_unfixable_unbound))
;;

(** -- Unfixable, multiple paths, one fixable, one unfixable -------------------
  #
p(X) :- q().      
  ^
  +----- 0 --------+---+
                   |   |
query() :- r(X), p(X). |
             |     ^   |
             +-----+   |
                       |
s(Y) :- p(X).          |
          |            |
          +-- 0 -------+

----------------------------------------------------------------------------- *)

let prg_unfixable_multi =
  Program.Raw.program
    Clause.Raw.
      [ clause Lit.Raw.(lit pred_p Term.[ var "X" ]) Lit.Raw.[ lit pred_q [] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_r Term.[ var "X" ]; lit pred_p Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_s Term.[ var "Y" ])
          Lit.Raw.[ lit pred_p Term.[ var "X" ] ]
      ]
    [ pred_qry ]
;;

let unfixable_multi_expected =
  reset ();
  MonadCompile.Err.RangeViolations
    Violation.
      [ violation Dataflow.Dest.(DPred (pred_p, 0)) Tmvar.(from_string "X") ]
;;

let unfixable_multi () =
  Alcotest.(check output)
    "Unfixable on one path, fixable on other."
    (Error unfixable_multi_expected)
    (MonadCompile.eval RangeRepair.(apply prg_unfixable_multi));
  reset ()
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Fixable with guard" `Quick fixable_guard
    ; test_case "Fixable with knowledge" `Quick fixable_knowledge
    ; test_case "Fixable on multiple paths" `Quick fixable_multi
    ; test_case "Unfixable, unbound variable" `Quick unfixable_unbound
    ; test_case
        "Unfixable on one path, fixable on other."
        `Quick
        unfixable_multi
    ]
;;
