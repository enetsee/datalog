open Core_kernel
open Core

let testable_constraint = Constraint.(Alcotest.testable pp equal)
let testable_schedule = Schedule.(Alcotest.testable pp equal)

let testable_orderings =
  let pp =
    Fmt.(
      vbox @@ list ~sep:cut @@ hbox @@ brackets @@ list ~sep:comma Lit.Raw.pp)
  (* sort the order of the orderings but not the orderings! *)
  and eq xxs yys =
    let xxs' = List.sort ~compare:(List.compare Lit.Raw.compare) xxs
    and yys' = List.sort ~compare:(List.compare Lit.Raw.compare) yys in
    List.equal (List.equal Lit.Raw.equal) xxs' yys'
  in
  Alcotest.testable pp eq
;;

let pred_f = Pred.(logical ~arity:1 @@ Name.from_string "f")
let pred_g = Pred.(logical ~arity:3 @@ Name.from_string "g")
let pred_h = Pred.(logical ~arity:1 @@ Name.from_string "h")
let pred_i = Pred.(logical ~arity:1 @@ Name.from_string "i")
let pred_j = Pred.(logical ~arity:2 @@ Name.from_string "j")
let pred_r = Pred.(logical ~arity:2 @@ Name.from_string "r")
let cstr_f = Constraint.(of_list Atomic.[ of_list [ 0 ] ])
let cstr_g = Constraint.(of_list Atomic.[ of_list [ 0; 1 ]; of_list [ 0; 2 ] ])
let cstr_h = Constraint.(of_list Atomic.[ of_list [ 0 ] ])
let cstr_i = Constraint.(of_list Atomic.[ of_list [ 0 ] ])
let lit_f_fig2 = Lit.Raw.(lit pred_f Term.[ var "X" ])
let lit_g_fig2 = Lit.Raw.(lit pred_g Term.[ var "X"; var "Y"; var "Z" ])
let lit_h_fig2 = Lit.Raw.(lit pred_h Term.[ var "Z" ])
let lit_i_fig2 = Lit.Raw.(lit pred_i Term.[ var "X" ])
let lit_j_fig2 = Lit.Raw.(lit pred_j Term.[ var "X"; var "W" ])

(** -- Figure 2 from `Automatic reordering for dataflow safety in Datalog`  ----

r(?Y,?Z) :- f{+}(?X), g{++?,+?+}(?X,?Y,?Z), h{+}(?Z), i(X), j(X,W).

{{0}, {1}}

() {}
L (i, j) {}
   L (f) {}
   + (g, h) {Z}
      L (g) {Y}
         L (h) {Y}

----------------------------------------------------------------------------- *)
let cl_fig2 =
  Clause.Raw.clause
    Lit.Raw.(lit pred_r Term.[ var "Y"; var "Z" ])
    [ lit_f_fig2; lit_g_fig2; lit_h_fig2; lit_i_fig2; lit_j_fig2 ]
;;

let cstrs_fig2 =
  Pred.Map.of_alist_exn [ pred_f, cstr_f; pred_g, cstr_g; pred_h, cstr_h ]
;;

let sched_fig2 = Schedule.of_clause cl_fig2 ~cstrs:cstrs_fig2

let clause_constraint_fig2 () =
  Alcotest.(check testable_constraint)
    "Clause constraint for Figure 2. from paper"
    Constraint.(of_list Atomic.[ of_list [ 0 ]; of_list [ 1 ] ])
    Schedule.(extract sched_fig2)
;;

(** 
bb

i, j, f, g, h
i, j, f, h, g
j, i, f, g, h
j, i, f, h, g

*)
let clause_orderings_bb_fig_2 () =
  Alcotest.(check testable_orderings)
    "Paths compatible with {b,b} for Figure 2. from paper"
    [ [ lit_i_fig2; lit_j_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ; [ lit_i_fig2; lit_j_fig2; lit_f_fig2; lit_h_fig2; lit_g_fig2 ]
    ; [ lit_j_fig2; lit_i_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ; [ lit_j_fig2; lit_i_fig2; lit_f_fig2; lit_h_fig2; lit_g_fig2 ]
    ]
    Schedule.(orderings sched_fig2 ~bpatt:Binding.(from_list [ Bound; Bound ]))
;;

(**
bf

i, j, f, g, h
j, i, f, g, h 
*)

let clause_orderings_bf_fig_2 () =
  Alcotest.(check testable_orderings)
    "Paths compatible with {b,b} for Figure 2. from paper"
    [ [ lit_i_fig2; lit_j_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ; [ lit_j_fig2; lit_i_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ]
    Schedule.(orderings sched_fig2 ~bpatt:Binding.(from_list [ Bound; Free ]))
;;

(** 
fb

i, j, f, g, h
i, j, f, h, g
j, i, f, g, h
j, i, f, h, g 
*)
let clause_orderings_fb_fig_2 () =
  Alcotest.(check testable_orderings)
    "Paths compatible with {b,b} for Figure 2. from paper"
    [ [ lit_i_fig2; lit_j_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ; [ lit_i_fig2; lit_j_fig2; lit_f_fig2; lit_h_fig2; lit_g_fig2 ]
    ; [ lit_j_fig2; lit_i_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ; [ lit_j_fig2; lit_i_fig2; lit_f_fig2; lit_h_fig2; lit_g_fig2 ]
    ]
    Schedule.(orderings sched_fig2 ~bpatt:Binding.(from_list [ Free; Bound ]))
;;

(** ff *)
let clause_orderings_ff_fig_2 () =
  Alcotest.(check testable_orderings)
    "Paths compatible with {f,f} for Figure 2. from paper"
    []
    Schedule.(orderings sched_fig2 ~bpatt:Binding.(from_list [ Free; Free ]))
;;

(** -- Stuck construction ------------------------------------------------------

f(X) :- h{+}(Y), i{+}(X).

----------------------------------------------------------------------------- *)
let cl_stuck =
  Clause.Raw.clause
    Lit.Raw.(lit pred_f Term.[ var "X" ])
    Lit.Raw.[ lit pred_h Term.[ var "Y" ]; lit pred_i Term.[ var "X" ] ]
;;

let cstrs_stuck = Pred.Map.of_alist_exn [ pred_i, cstr_i; pred_h, cstr_h ]
let sched_stuck = Schedule.of_clause cl_stuck ~cstrs:cstrs_stuck

let stuck_constraint () =
  Alcotest.(check testable_constraint)
    "Clause constraint for stuck schedule graph"
    Constraint.ill
    Schedule.(extract sched_stuck)
;;

let clause_orderings_stuck () =
  Alcotest.(check testable_orderings)
    "Paths compatible with {b} for stuck schedule graph"
    []
    Schedule.(orderings sched_stuck ~bpatt:Binding.(from_list [ Bound ]))
;;

(** -- Wildcard, required ------------------------------------------------------

f(X) :- i(X), h{+}(_).

----------------------------------------------------------------------------- *)
let cl_wildcard_req =
  Clause.Raw.clause
    Lit.Raw.(lit pred_f Term.[ var "X" ])
    Lit.Raw.[ lit_j_fig2; lit pred_h Term.[ wild () ] ]
;;

let cstrs_wildcard_req = Pred.Map.of_alist_exn [ pred_h, cstr_h ]

let sched_wildcard_req =
  Schedule.of_clause cl_wildcard_req ~cstrs:cstrs_wildcard_req
;;

let wildcard_req_constraint () =
  Alcotest.(check testable_constraint)
    "Clause constraint for schedule graph stuck on required wilcard"
    Constraint.ill
    Schedule.(extract sched_wildcard_req)
;;

(** -- Wildcard, optional ------------------------------------------------------

f(X) :- i(X), h(_).

----------------------------------------------------------------------------- *)

let sched_wildcard_opt =
  Schedule.of_clause cl_wildcard_req ~cstrs:Pred.Map.empty
;;

let wildcard_opt_constraint () =
  Alcotest.(check testable_constraint)
    "Clause constraint for schedule graph with optional wilcard"
    Constraint.trivial
    Schedule.(extract sched_wildcard_opt)
;;

(** -- Negation, schedulable ---------------------------------------------------

r(Y,X) :- f{+}(?X), not g(?X,?Y,?Z), h(?Z)

Negated literals must be fully bound; this case is schedulable since `X` occurs
in the head of the clause and `Z` is not required in `h(Z)`.

----------------------------------------------------------------------------- *)
let cl_neg_ok =
  Clause.Raw.clause
    Lit.Raw.(lit pred_r Term.[ var "Y"; var "X" ])
    [ lit_f_fig2; Lit.Raw.neg lit_g_fig2; lit_h_fig2 ]
;;

let cstrs_neg_ok = Pred.Map.of_alist_exn [ pred_f, cstr_f ]
let sched_neg_ok = Schedule.of_clause cl_neg_ok ~cstrs:cstrs_neg_ok

let neg_ok_constraint () =
  Alcotest.(check testable_constraint)
    "Negated literal, schedulable"
    Constraint.(of_list Atomic.[ of_list [ 0; 1 ] ])
    Schedule.(extract sched_neg_ok)
;;

(** -- Negation, unschedulable -------------------------------------------------

r(Y,X) :- f{+}(?X), not g(?X,?Y,?Z), h{+}(?Z)

This case is unschedulable since `Z` is required in both `g(X,Y,Z)` and `h(Z)`
and does not occur in the head of the calsue.
----------------------------------------------------------------------------- *)

let cstrs_neg_bad = Pred.Map.of_alist_exn [ pred_f, cstr_f; pred_h, cstr_h ]
let sched_neg_bad = Schedule.of_clause cl_neg_ok ~cstrs:cstrs_neg_bad

let neg_bad_constraint () =
  Alcotest.(check testable_constraint)
    "Negated literal, unschedulable"
    Constraint.ill
    Schedule.(extract sched_neg_bad)
;;

(** -- Negation, constraints overriden, unschedulable --------------------------

r(Y,X) :- f{+}(?X), not g{++?,+?+}(?X,?Y,?Z), h{+}(?Z)

----------------------------------------------------------------------------- *)

let cstrs_neg_bad_override =
  Pred.Map.of_alist_exn [ pred_f, cstr_f; pred_g, cstr_g; pred_h, cstr_h ]
;;

let sched_neg_bad_override =
  Schedule.of_clause cl_neg_ok ~cstrs:cstrs_neg_bad_override
;;

let neg_bad_override_constraint () =
  Alcotest.(check testable_constraint)
    "Negated literal, constraint overriden, unschedulable"
    Constraint.ill
    Schedule.(extract sched_neg_bad_override)
;;

(** -- Effectful, schedulable --------------------------------------------------


r(Y,X) :- n{+?}<RW>(Z,Y), m{+?}<HTTP>(X,Z).

Without effects, we can simply move m in front of n so `Z` becomes bound. With
effects we have to maintain the relative order of extralogical predicates
where there effects sets intersect.

Here we can still move `m` ahead of `n` since  they have non-intersecting 
effects.

----------------------------------------------------------------------------- *)

let pred_m =
  Pred.(extralogical ~eff:Eff.[ EffHTTP ] ~arity:2 @@ Name.from_string "m")
;;

let cstr_m = Constraint.(of_list Atomic.[ of_list [ 0 ] ])

let pred_n =
  Pred.(extralogical ~eff:Eff.[ EffRW ] ~arity:2 @@ Name.from_string "n")
;;

let cstr_n = Constraint.(of_list Atomic.[ of_list [ 0 ] ])

let pred_o =
  Pred.(
    extralogical ~eff:Eff.[ EffRW; EffHTTP ] ~arity:2 @@ Name.from_string "o")
;;

let cstr_o = Constraint.(of_list Atomic.[ of_list [ 0 ] ])

let cl_eff_ok =
  Clause.Raw.clause
    Lit.Raw.(lit pred_r Term.[ var "Y"; var "X" ])
    Lit.Raw.
      [ lit pred_n Term.[ var "Z"; var "Y" ]
      ; lit pred_m Term.[ var "X"; var "Z" ]
      ]
;;

let cstr_eff =
  Pred.Map.of_alist_exn [ pred_m, cstr_m; pred_n, cstr_n; pred_o, cstr_o ]
;;

let sched_eff_ok = Schedule.of_clause cl_eff_ok ~cstrs:cstr_eff

let eff_ok_constraint () =
  Alcotest.(check testable_constraint)
    "Effectful literals, non-intersecting effects, schedulable"
    Constraint.(of_list Atomic.[ of_list [ 1 ] ])
    Schedule.(extract sched_eff_ok)
;;

(** -- Effectful, unschedulable ------------------------------------------------


r(Y,X) :- n{+?}<RW>(Z,Y), o{+?}<RW,HTTP>(X,Z).

Here we can't still move `om` ahead of `n` since  they have intersecting 
effects.
----------------------------------------------------------------------------- *)

let cl_eff_bad =
  Clause.Raw.clause
    Lit.Raw.(lit pred_r Term.[ var "Y"; var "X" ])
    Lit.Raw.
      [ lit pred_n Term.[ var "Z"; var "Y" ]
      ; lit pred_o Term.[ var "X"; var "Z" ]
      ]
;;

let sched_eff_bad = Schedule.of_clause cl_eff_bad ~cstrs:cstr_eff

let eff_bad_constraint () =
  Alcotest.(check testable_constraint)
    "Effectful literals, intersecting effects, unschedulable"
    Constraint.ill
    Schedule.(extract sched_eff_bad)
;;

(** Simple negation *)

let cl_simple_neg =
  Clause.Raw.clause
    Lit.Raw.(lit pred_r Term.[ var "X"; var "Y" ])
    Lit.Raw.[ lit ~pol:Neg pred_j Term.[ var "X"; var "Y" ] ]
;;

let sched_simple_neg = Schedule.of_clause cl_simple_neg ~cstrs:Pred.Map.empty

let simple_neg_constraint () =
  Alcotest.(check testable_constraint)
    "Simple negation, schedulable"
    Constraint.(of_list Atomic.[ of_list [ 0; 1 ] ])
    Schedule.(extract sched_simple_neg)
;;

(** Recursion  *)

let cl_recursion =
  Clause.Raw.clause
    Lit.Raw.(lit pred_r Term.[ var "X"; var "Y" ])
    Lit.Raw.
      [ lit pred_j Term.[ var "X"; var "Z" ]
      ; lit pred_r Term.[ var "Z"; var "Y" ]
      ]
;;

let sched_recursion = Schedule.of_clause cl_recursion ~cstrs:Pred.Map.empty

let recursion_constraint () =
  Alcotest.(check testable_constraint)
    "Recursion, schedulable"
    Constraint.trivial
    Schedule.(extract sched_recursion)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Clause constraint for Fig 2" `Quick clause_constraint_fig2
    ; test_case "Orderings for {b,b} on Fig 2" `Quick clause_orderings_bb_fig_2
    ; test_case "Orderings for {b,f} on Fig 2" `Quick clause_orderings_bf_fig_2
    ; test_case "Orderings for {f,b} on Fig 2" `Quick clause_orderings_fb_fig_2
    ; test_case "Orderings for {f,f} on Fig 2" `Quick clause_orderings_ff_fig_2
    ; test_case
        "Clause constraint for schedule graph stuck on requied wildcard"
        `Quick
        wildcard_req_constraint
    ; test_case
        "Clause constraint for schedule graph with optional wilcard"
        `Quick
        wildcard_opt_constraint
    ; test_case
        "Clause constraint for stuck schedule graph"
        `Quick
        stuck_constraint
    ; test_case
        "Orderings for {b} on stuck schedule graph "
        `Quick
        clause_orderings_stuck
    ; test_case "Negated literal, schedulable" `Quick neg_ok_constraint
    ; test_case "Negated literal, unschedulable" `Quick neg_bad_constraint
    ; test_case
        "Negated literal, constraint overriden, unschedulable"
        `Quick
        neg_bad_override_constraint
    ; test_case
        "Effectful literals, non-intersecting effects, schedulable"
        `Quick
        eff_ok_constraint
    ; test_case
        "Effectful literals, intersecting effects, unschedulable"
        `Quick
        eff_bad_constraint
    ; test_case "Simple negation" `Quick simple_neg_constraint
    ; test_case "Recursion" `Quick recursion_constraint
    ]
;;
