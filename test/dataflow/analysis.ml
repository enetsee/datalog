open Core
open Dataflow

let pred_a = Pred.(pred ~arity:1 @@ Name.from_string "a")
let pred_p = Pred.(pred ~arity:1 @@ Name.from_string "p")
let pred_q = Pred.(pred ~arity:1 @@ Name.from_string "q")
let pred_r = Pred.(pred ~arity:2 @@ Name.from_string "r")
let pred_s = Pred.(pred ~arity:1 @@ Name.from_string "s")
let pred_qry = Pred.(pred ~arity:0 @@ Name.from_string "query")
let pred_qry2 = Pred.(pred ~arity:0 @@ Name.from_string "query2")
let destQ = Dest.DLit (Lit.Raw.lit pred_q Term.[ var "X" ], 0)
let destP = Dest.DPred (pred_p, 0)
let destR = Dest.DLit (Lit.Raw.lit pred_r Term.[ var "X"; var "X" ], 0)
let testable_src = Src.(Alcotest.testable pp equal)

(** -- Covering constant -------------------------------------------------------
 
p(X) :- q(X).
  ^
  |
  -----------+
             |
query() :- p(1).
 
----------------------------------------------------------------------------- *)
let prg_const =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_p Term.[ sym @@ Symbol.int 1 ] ]
      ]
;;

let covering_const_expect = Some [ Src.SConst (Const.CSym (Symbol.int 1)) ]

let constant () =
  Alcotest.(check @@ option @@ list testable_src)
    "single covering constant symbol"
    covering_const_expect
    Graph.(coveringPositives ~dest:destQ @@ from_prog prg_const [ pred_qry ])
;;

(** -- Covering constant with additional dead path -----------------------------

p(X) :- q(X).
  ^
  +----------+
             |  
query() :- p(1).
s(X) :- p(X).
|       
0

----------------------------------------------------------------------------- *)
let prg_dead_path =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause Lit.Raw.(lit pred_qry []) Lit.Raw.[ lit pred_p Term.[ int 1 ] ]
      ; clause
          Lit.Raw.(lit pred_s Term.[ var "X" ])
          Lit.Raw.[ lit pred_p Term.[ var "X" ] ]
      ]
;;

let dead_path () =
  Alcotest.(check @@ option @@ list testable_src)
    "single covering constant with dead path"
    covering_const_expect
    Graph.(
      coveringPositives ~dest:destQ @@ from_prog prg_dead_path [ pred_qry ])
;;

(** -- Covering constant but predicate `p` also exposed as a query -------------

  0
  |
p(X) :- q(X).
  ^
  +----------+
             |
query() :- p(1).
 
----------------------------------------------------------------------------- *)

let const_exposed () =
  Alcotest.(check @@ option @@ list testable_src)
    "single covering constant with dead path"
    None
    Graph.(
      coveringPositives ~dest:destQ @@ from_prog prg_const [ pred_p; pred_qry ])
;;

(** -- Covering wildcard -------------------------------------------------------

p(X) :- q(X).
  ^
  +----------+
             |
query() :- p(_).

----------------------------------------------------------------------------- *)
let prg_wild =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_p Term.[ wild None ] ]
      ]
;;

let wild () =
  Alcotest.(check @@ option @@ list testable_src)
    "single covering constant wildcard"
    (Some [ Src.SConst (Const.CWild None) ])
    Graph.(coveringPositives ~dest:destQ @@ from_prog prg_wild [ pred_qry ])
;;

(** -- Single open path --------------------------------------------------------

  +---0---+
  |       v
p(X) :- q(X).
  ^------------+
query :- q(X). |
           |   |
           0---+

----------------------------------------------------------------------------- *)
let prg_single_open =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause Lit.Raw.(lit pred_qry []) Lit.Raw.[ lit pred_p Term.[ var "X" ] ]
      ]
;;

let single_open () =
  Alcotest.(check @@ option @@ list testable_src)
    "Single path, no covering constants"
    None
    Graph.(
      coveringPositives ~dest:destQ @@ from_prog prg_single_open [ pred_qry ])
;;

(** -- Two paths, one open -----------------------------------------------------

p(X) :- q(X).
  ^
  +-------- 0 ------+- 0 +
                    |    |
query1() :- a(X), p(X).  |
              |     ^    |
              +-----+    |
query() :- p(X).         |
             |           |
             +-----------+

----------------------------------------------------------------------------- *)
let prg_multi_half_open =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_qry2 [])
          Lit.Raw.[ lit pred_a Term.[ var "X" ]; lit pred_p Term.[ var "X" ] ]
      ; clause Lit.Raw.(lit pred_qry []) Lit.Raw.[ lit pred_p Term.[ var "X" ] ]
      ]
;;

let multi_half_open () =
  Alcotest.(check @@ option @@ list testable_src)
    "Two paths, one open"
    None
    Graph.(
      coveringPositives ~dest:destQ
      @@ from_prog prg_multi_half_open [ pred_qry ])
;;

(** -- Variable aliased in head , both covered ---------------------------------

  +--+-------+
  |  |       v
r(X, X) :- q(X).
  ^  ^
  +--+-------------+--+
                   |  |
query() :- a(X), r(X, 1).
             |     ^
             +-----+

----------------------------------------------------------------------------- *)
let prg_alias_head_closed =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_r Term.[ var "X"; var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.
            [ lit pred_a Term.[ var "X" ]; lit pred_r Term.[ var "X"; int 1 ] ]
      ]
;;

let alias_head_closed () =
  Alcotest.(check @@ option @@ list testable_src)
    "Variable aliased in head , both covered"
    (Some
       [ Src.SLit (Lit.Raw.lit pred_a Term.[ var "X" ], 0)
       ; Src.SConst (Const.CSym (Symbol.int 1))
       ])
    Graph.(
      coveringPositives ~dest:destQ
      @@ from_prog prg_alias_head_closed [ pred_qry ])
;;

(** -- Variable aliased in head , one covered, one open ------------------------

  +--+--- 0 -+
  0  |       v
r(X, X) :- q(X).
  ^  ^
  |  +----------+
  +----------0  |
             |  |
query() :- r(X, 1).

----------------------------------------------------------------------------- *)
let prg_alias_head_half_open =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_r Term.[ var "X"; var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_r Term.[ var "X"; int 1 ] ]
      ]
;;

let alias_head_half_open () =
  Alcotest.(check @@ option @@ list testable_src)
    "Variable aliased in head , one covered, one open"
    None
    Graph.(
      coveringPositives ~dest:destQ
      @@ from_prog prg_alias_head_half_open [ pred_qry ])
;;

(** -- Variable aliased in body , covered by preceeding literal ----------------

query() :- a(X), r(X, 1).
             |     ^
             +-----+                                            

----------------------------------------------------------------------------- *)
let prg_alias_body =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.
            [ lit pred_a Term.[ var "X" ]
            ; lit pred_r Term.[ var "X"; var "X" ]
            ]
      ]
;;

let alias_body () =
  Alcotest.(check @@ option @@ list testable_src)
    "Variable aliased in body , covered by preceeding literal"
    (Some [ Src.SLit (Lit.Raw.lit pred_a Term.[ var "X" ], 0) ])
    Graph.(
      coveringPositives ~dest:destR @@ from_prog prg_alias_body [ pred_qry ])
;;

(** -- Multiple paths, one directly covered, one indirectly covered ------------

  +-------+
  |       v
s(X) :- q(X).
  ^
  +----------------+
                   |
  +-------+        |
  |       v        |
p(X) :- s(X).      |
  ^       |        |
  |       +--------+
  +----------+     |
             |     |
query() :- p(1), s(2).

----------------------------------------------------------------------------- *)
let prg_indirection =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_s Term.[ var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_s Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_qry [])
          Lit.Raw.[ lit pred_p Term.[ int 1 ]; lit pred_s Term.[ int 2 ] ]
      ]
;;

let indirection () =
  Alcotest.(check @@ option @@ list testable_src)
    "Multiple paths, one directly covered, one indirectly covered"
    (Some
       [ Src.SConst (Const.CSym (Symbol.int 2))
       ; Src.SConst (Const.CSym (Symbol.int 1))
       ])
    Graph.(
      coveringPositives ~dest:destQ @@ from_prog prg_indirection [ pred_qry ])
;;

(** -- Multiple paths, recursive def, one directly covered, one indirectly via
       preceeding literal 

  +-------+
  |       v
p(X) :- q(X).
  ^
  +-------------+----+
  +-------+     |    |
  |       v     |    |
p(1) :- a(Y), p(Y).  |
          +     ^    |
          +-----+    |
             +-------+
             |
query() :- p(1).

----------------------------------------------------------------------------- *)

let prg_rec_closed =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_p Term.[ int 1 ])
          Lit.Raw.[ lit pred_a Term.[ var "Y" ]; lit pred_p Term.[ var "Y" ] ]
      ; clause Lit.Raw.(lit pred_qry []) Lit.Raw.[ lit pred_p Term.[ int 1 ] ]
      ]
;;

let rec_closed () =
  Alcotest.(check @@ option @@ list testable_src)
    "Multiple paths, recursive def, one directly covered, one indirectly"
    (Some
       [ Src.SConst (Const.CSym (Symbol.int 1))
       ; Src.SLit (Lit.Raw.lit pred_a Term.[ var "Y" ], 0)
       ])
    Graph.(
      coveringPositives ~dest:destQ @@ from_prog prg_rec_closed [ pred_qry ])
;;

(** -- Multiple paths, recursive def, one directly covered, one indirectly but
       by the same literal

  +-------+
  |       v
p(X) :- q(X).
  ^
  +--------------+
  +-------+      |
  |       v      |
p(X) :- p(X).    |
  ^       |      |
  |       +------+
  |              |
  ---------+-----+
           |
query :- p(1).

 ----------------------------------------------------------------------------- *)

let prg_rec_closed_indiff =
  Program.Raw.program
    Clause.Raw.
      [ clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_q Term.[ var "X" ] ]
      ; clause
          Lit.Raw.(lit pred_p Term.[ var "X" ])
          Lit.Raw.[ lit pred_p Term.[ var "X" ] ]
      ; clause Lit.Raw.(lit pred_qry []) Lit.Raw.[ lit pred_p Term.[ int 1 ] ]
      ]
;;

let rec_closed_indiff () =
  Alcotest.(check @@ option @@ list testable_src)
    "Multiple paths, recursive def, one directly covered, one indirectly"
    (Some [ Src.SConst (Const.CSym (Symbol.int 1)) ])
    Graph.(
      coveringPositives ~dest:destQ
      @@ from_prog prg_rec_closed_indiff [ pred_qry ])
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Covering constant symbol" `Quick constant
    ; test_case "Covering constant with additional dead path" `Quick dead_path
    ; test_case "Covering constant but predicate exposed" `Quick const_exposed
    ; test_case "Covering constant wildcard" `Quick wild
    ; test_case "No covering literal, single path" `Quick single_open
    ; test_case
        "No covering literal, multiple paths, one open"
        `Quick
        single_open
    ; test_case
        "Variable aliased in head , both covered"
        `Quick
        alias_head_closed
    ; test_case
        "Variable aliased in head , one covered, one open"
        `Quick
        alias_head_half_open
    ; test_case
        "Variable aliased in body , covered by preceeding literal"
        `Quick
        alias_body
    ; test_case
        "Multiple paths, one directly covered, one indirectly covered"
        `Quick
        indirection
    ; test_case
        "Recursive def, one directly covered by lit, one indirectly by var"
        `Quick
        rec_closed
    ; test_case
        "Recursive def, one directly covered, one indirectly but by the same \
         literal"
        `Quick
        rec_closed_indiff
    ]
;;
