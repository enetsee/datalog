(** The range restriction violation check and repair assumes that all 
    dead clauses have already been removed from the program 
*)
open Core_kernel

open Core

module Err = struct
  type t = Violation.t list
end

module Fail = struct
  include Effect.MonadFail.Make (Err)

  let equal eq_a = Result.equal eq_a (List.equal Violation.equal)
  let pp pp_a = Fmt.(result ~ok:pp_a ~error:(list ~sep:comma Violation.pp))
end

module M = struct
  include Effect.StateT.Make_with_state (Int) (Fail)

  let fresh_guardsym =
    get
    >>= fun i ->
    put (i + 1)
    >>= fun _ -> return @@ Name.from_string ("guard" ^ string_of_int i)
  ;;

  let err_range_violations vs = lift @@ Fail.fail vs
end

module RangeM = RangeRepair.Make (M)

let pred_p = Pred.(pred ~arity:1 @@ Name.from_string "p")
let pred_q = Pred.(pred ~arity:0 @@ Name.from_string "q")
let pred_r = Pred.(pred ~arity:1 @@ Name.from_string "r")
let pred_s = Pred.(pred ~arity:1 @@ Name.from_string "s")
let pred_qry = Pred.(pred ~arity:0 @@ Name.from_string "query")
let pred_qry2 = Pred.(pred ~arity:0 @@ Name.from_string "query2")
let i = ref 0
let reset () = i := 0

let fresh_pred_sym pfx =
  let sym = pfx ^ string_of_int !i in
  i := !i + 1;
  Name.from_string sym
;;

let mk_guard () = Pred.(pred ~arity:1 @@ fresh_pred_sym "guard")

let output =
  let pp_a = Fmt.(pair Program.Raw.pp Knowledge.Base.pp)
  and eq_a = Tuple2.equal ~eq1:Program.Raw.equal ~eq2:Knowledge.Base.equal in
  Fail.(Alcotest.testable (pp pp_a) (equal eq_a))
;;

let mk_repair msg expect prg_in queries =
  let f () =
    Alcotest.check
      output
      msg
      expect
      M.(eval ~init:0 @@ RangeM.fix_program prg_in queries)
  in
  Alcotest.test_case msg `Quick f
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
  , Knowledge.Base.empty )
;;

let fixable_guard =
  mk_repair
    "Fixable with guard"
    (Ok fixable_guard_expected)
    prg_fixable_guard
    [ pred_qry ]
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
  , Knowledge.(Base.singleton @@ knowledge pred_grd [ Symbol.from_int 1 ]) )
;;

let fixable_knowledge =
  mk_repair
    "Fixable with knowledge"
    (Ok fixable_knowledge_expected)
    prg_fixable_knowledge
    [ pred_qry ]
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
  , Knowledge.(Base.singleton @@ knowledge pred_grd [ Symbol.from_int 1 ]) )
;;

let fixable_multi =
  mk_repair
    "Fixable on multiple paths"
    (Ok fixable_multi_expected)
    prg_fixable_multi
    [ pred_qry ]
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
;;

let unfixable_unbound_expected =
  reset ();
  [ Violation.violation
      Dataflow.Dest.(DPred (pred_p, 0))
      Tmvar.(from_string "X")
  ]
;;

let unfixable_unbound =
  mk_repair
    "Unfixable, unbound variable"
    (Error unfixable_unbound_expected)
    prg_unfixable_unbound
    [ pred_qry ]
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
;;

let unfixable_multi_expected =
  reset ();
  Violation.
    [ violation Dataflow.Dest.(DPred (pred_p, 0)) Tmvar.(from_string "X") ]
;;

let unfixable_multi =
  mk_repair
    "Unfixable on one path, fixable on other."
    (Error unfixable_multi_expected)
    prg_unfixable_multi
    [ pred_qry ]
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  [ fixable_guard
  ; fixable_knowledge
  ; fixable_multi
  ; unfixable_unbound
  ; unfixable_multi
  ]
;;
