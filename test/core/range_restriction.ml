(** The range restriction violation check and repair assumes that all 
    dead clauses have already been removed from the program 
*)
open Core_kernel

open Core
open Raw
let pred_p = Pred.(logical ~arity:1 @@ Name.from_string "p")
let pred_q = Pred.(logical ~arity:0 @@ Name.from_string "q")
let pred_r = Pred.(logical ~arity:1 @@ Name.from_string "r")
let pred_s = Pred.(logical ~arity:1 @@ Name.from_string "s")
let pred_qry = Pred.(logical ~arity:0 @@ Name.from_string "query")
let pred_qry2 = Pred.(logical ~arity:0 @@ Name.from_string "query2")


(* TODO:  monadic approach *)
let fresh_pred_sym =
  let i = ref 0 in
  fun pfx ->
    let sym = pfx ^ string_of_int !i in
    i := !i + 1;
    Pred.Name.from_string sym


let mk_guard () = Pred.(logical ~arity:1 @@ fresh_pred_sym "guard")


(** Testable instance for violations *)
let testable_violation = 
  RangeRepair.Violation.(
  Alcotest.testable pp equal  
  ) 

let testable_prg = Program.(Alcotest.testable pp equal)


let pp_prg_kb ppf (prg,kb)= 
  Fmt.(vbox @@ pair ~sep:cut Program.pp @@ list ~sep:cut Knowledge.pp)
    ppf 
    (prg , Knowledge.Set.to_list kb)

let eq_prg_kb = Tuple2.equal ~eq1:Program.equal ~eq2:Knowledge.Set.equal

let testable_prg_kb = 
  Alcotest.testable pp_prg_kb eq_prg_kb


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
  Program.program
    Clause.[
      clause Lit.(lit pred_p Term.[var "X"]) Lit.[lit pred_q []]
      ; clause Lit.(lit pred_qry []) Lit.[lit pred_r Term.[var "X"]; lit pred_p Term.[var "X"]]
    ]
    [pred_qry]

let fixable_guard_expected = 
  let pred_grd = mk_guard () in
  Program.program
    Clause.[
      clause Lit.(lit pred_p Term.[var "X"]) 
        Lit.[lit pred_grd Term.[var "X"]; lit pred_q []]
      ; clause Lit.(lit pred_qry []) Lit.[lit pred_r Term.[var "X"]; lit pred_p Term.[var "X"]]
      ; clause Lit.(lit pred_grd Term.[var "X"]) Lit.[lit pred_r Term.[var "X"]]
    ]
    [pred_qry]
  , Knowledge.Set.empty 

let fixable_guard () = 
  Alcotest.(check @@ result testable_prg_kb @@ list testable_violation)
    "Fixable with guard"
    (Ok fixable_guard_expected)
    RangeRepair.(apply prg_fixable_guard)
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
  Program.program
    Clause.[
      clause Lit.(lit pred_p Term.[var "X"]) Lit.[lit pred_q []]
      ; clause Lit.(lit pred_qry []) Lit.[lit pred_p Term.[sym @@ Symbol.from_int 1]]
    ]
    [pred_qry]

let fixable_knowledge_expected = 
  let pred_grd = mk_guard () in
  Program.program
    Clause.[
      clause Lit.(lit pred_p Term.[var "X"]) 
        Lit.[lit pred_grd Term.[var "X"]; lit pred_q []]
      ; clause Lit.(lit pred_qry []) Lit.[lit pred_p Term.[sym @@ Symbol.from_int 1]]      
    ]
    [pred_qry],
  Knowledge.(Set.singleton @@  knowledge pred_grd [Symbol.from_int 1])

let fixable_knowledge () = 
  Alcotest.(check @@ result testable_prg_kb @@ list testable_violation)
    "Fixable with knowledge"
    (Ok fixable_knowledge_expected)
    RangeRepair.(apply prg_fixable_knowledge)
;;

(** -- Fixable, multiple paths -------------------------------------------------

----------------------------------------------------------------------------- *)
let prg_fixable_multi = 
  Program.program
    Clause.[
      clause Lit.(lit pred_p Term.[var "X"]) Lit.[lit pred_q []]
      ; clause Lit.(lit pred_qry []) Lit.[lit pred_r Term.[var "X"]; lit pred_p Term.[var "X"]]
      ; clause Lit.(lit pred_qry2 []) Lit.[lit pred_p Term.[sym @@ Symbol.from_int 1]]
    ]
    [pred_qry]

let fixable_multi_expected = 
  let pred_grd = mk_guard () in
  Program.program
    Clause.[
      clause Lit.(lit pred_p Term.[var "X"]) 
        Lit.[lit pred_grd Term.[var "X"]; lit pred_q []]
      ; clause Lit.(lit pred_qry []) Lit.[lit pred_r Term.[var "X"]; lit pred_p Term.[var "X"]]
      ; clause Lit.(lit pred_qry2 []) Lit.[lit pred_p Term.[sym @@ Symbol.from_int 1]]
      ; clause Lit.(lit pred_grd Term.[var "X"]) Lit.[lit pred_r Term.[var "X"]]
    ]
    [pred_qry]
  , Knowledge.(Set.singleton @@  knowledge pred_grd [Symbol.from_int 1])

let fixable_multi () = 
  Alcotest.(check @@ result testable_prg_kb @@ list testable_violation)
    "Fixable on multiple paths"
    (Ok fixable_multi_expected)
    RangeRepair.(apply prg_fixable_multi)
;;



(** -- Unfixable  --------------------------------------------------------------
  #
p(X) :- q().      
  
query() :- p(X), r(X).
             
      
This is currently unfixable even though  we could simply move `p(X)` in the
`query` clause to a position where `X` was bound.
----------------------------------------------------------------------------- *)

let prg_unfixable_unbound = 
  Program.program
    Clause.[
      clause Lit.(lit pred_p Term.[var "X"]) Lit.[lit pred_q []]
      ; clause Lit.(lit pred_qry []) Lit.[lit pred_p Term.[var "X"]; lit pred_r Term.[var "X"]]
    ]
    [pred_qry]

let unfixable_unbound_expected = 
  RangeRepair.Violation.[ 
      violation Dataflow.Dest.(DPred(pred_p,0)) Tmvar.(from_string "X") 
  ]
let unfixable_unbound () = 
  Alcotest.(check @@ result testable_prg_kb @@ list testable_violation)
    "Unfixable, unbound variable"
    (Error unfixable_unbound_expected)
    RangeRepair.(apply prg_unfixable_unbound)




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
  Program.program
    Clause.[
      clause Lit.(lit pred_p Term.[var "X"]) Lit.[lit pred_q []]
      ; clause Lit.(lit pred_qry []) Lit.[lit pred_r Term.[var "X"]; lit pred_p Term.[var "X"]]
      ; clause Lit.(lit pred_s Term.[var "Y"]) Lit.[lit pred_p Term.[var "X"]]
    ]
    [pred_qry]

let unfixable_multi_expected = 
  RangeRepair.Violation.[ 
      violation Dataflow.Dest.(DPred(pred_p,0)) Tmvar.(from_string "X") 
  ]
  

let unfixable_multi () = 
  Alcotest.(check @@ result testable_prg_kb @@ list testable_violation)
    "Unfixable on one path, fixable on other."
    (Error unfixable_multi_expected)
    RangeRepair.(apply prg_unfixable_multi)



(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Fixable with guard" `Quick fixable_guard 
    ; test_case "Fixable with knowledge" `Quick fixable_knowledge
    ; test_case "Fixable on multiple paths" `Quick fixable_multi
    ; test_case "Unfixable, unbound variable" `Quick unfixable_unbound
    ; test_case "Unfixable on one path, fixable on other." `Quick unfixable_multi
    ]

