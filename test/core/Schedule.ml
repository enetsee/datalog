open Core_kernel
open Core

module M = struct
  include Effect.State.Make (TypingEnv)

  let get_pred_constraint name =
    get
    >>= fun tyenv ->
    return
    @@ Option.value ~default:Constraint.trivial
    @@ TypingEnv.find_pred_constraint ~name tyenv
  ;;

  let set_pred_constraint name cstr =
    get
    >>= fun tyenv ->
    put @@ TypingEnv.update_pred_constraint_exn tyenv ~name ~cstr
  ;;

  let get_pred_effects name =
    get
    >>= fun tyenv ->
    return
    @@ Option.value ~default:Eff.Set.empty
    @@ TypingEnv.find_pred_effects ~name tyenv
  ;;
end

module ScheduleM = Schedule.Make (M)

let mk_constraint test_name expect_cstr clause typing_env =
  Alcotest.check
    Testable.cnstr
    test_name
    expect_cstr
    M.(
      eval ~init:typing_env
      @@ map ~f:Schedule.extract
      @@ ScheduleM.of_clause clause)
;;

let mk_ordering test_name expect_ord clause typing_env bpatt =
  Alcotest.check
    Testable.orderings
    test_name
    expect_ord
    M.(
      eval ~init:typing_env
      @@ map ~f:Schedule.(orderings ~bpatt)
      @@ ScheduleM.of_clause clause)
;;

let name_f = Name.from_string "f"
let name_g = Name.from_string "g"
let name_h = Name.from_string "h"
let name_i = Name.from_string "i"
let name_j = Name.from_string "j"
let name_r = Name.from_string "r"
let pred_f = Pred.(pred ~arity:1 name_f)
let pred_g = Pred.(pred ~arity:3 name_g)
let pred_h = Pred.(pred ~arity:1 name_h)
let pred_i = Pred.(pred ~arity:1 name_i)
let pred_j = Pred.(pred ~arity:2 name_j)
let pred_r = Pred.(pred ~arity:2 name_r)
let cstr_f = Constraint.(of_list Atomic.[ of_list [ 0 ] ])
let cstr_g = Constraint.(of_list Atomic.[ of_list [ 0; 1 ]; of_list [ 0; 2 ] ])
let cstr_h = Constraint.(of_list Atomic.[ of_list [ 0 ] ])
let cstr_i = Constraint.(of_list Atomic.[ of_list [ 0 ] ])

let info_f =
  TypingEnv.{ cstr = cstr_f; nature = Logical; typing = Typing.bottom }
;;

let info_g =
  TypingEnv.{ cstr = cstr_g; nature = Logical; typing = Typing.bottom }
;;

let info_h =
  TypingEnv.{ cstr = cstr_h; nature = Logical; typing = Typing.bottom }
;;

let info_i =
  TypingEnv.{ cstr = cstr_i; nature = Logical; typing = Typing.bottom }
;;

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

let tyenv_fig2 =
  TypingEnv.(
    add_pred ~name:name_h ~info:info_h
    @@ add_pred ~name:name_g ~info:info_g
    @@ add_pred ~name:name_f ~info:info_f
    @@ empty)
;;

let clause_constraint_fig2 () =
  mk_constraint
    "Clause constraint for Figure 2. from paper"
    Constraint.(of_list Atomic.[ of_list [ 0 ]; of_list [ 1 ] ])
    cl_fig2
    tyenv_fig2
;;

(** 
bb

i, j, f, g, h
i, j, f, h, g
j, i, f, g, h
j, i, f, h, g

*)
let clause_orderings_bb_fig_2 () =
  mk_ordering
    "Paths compatible with {b,b} for Figure 2. from paper"
    [ [ lit_i_fig2; lit_j_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ; [ lit_i_fig2; lit_j_fig2; lit_f_fig2; lit_h_fig2; lit_g_fig2 ]
    ; [ lit_j_fig2; lit_i_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ; [ lit_j_fig2; lit_i_fig2; lit_f_fig2; lit_h_fig2; lit_g_fig2 ]
    ]
    cl_fig2
    tyenv_fig2
    Binding.(from_list [ Bound; Bound ])
;;

(**
bf

i, j, f, g, h
j, i, f, g, h 
*)
let clause_orderings_bf_fig_2 () =
  mk_ordering
    "Paths compatible with {b,f} for Figure 2. from paper"
    [ [ lit_i_fig2; lit_j_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ; [ lit_j_fig2; lit_i_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ]
    cl_fig2
    tyenv_fig2
    Binding.(from_list [ Bound; Free ])
;;

(** 
fb

i, j, f, g, h
i, j, f, h, g
j, i, f, g, h
j, i, f, h, g 
*)
let clause_orderings_fb_fig_2 () =
  mk_ordering
    "Paths compatible with {f,b} for Figure 2. from paper"
    [ [ lit_i_fig2; lit_j_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ; [ lit_i_fig2; lit_j_fig2; lit_f_fig2; lit_h_fig2; lit_g_fig2 ]
    ; [ lit_j_fig2; lit_i_fig2; lit_f_fig2; lit_g_fig2; lit_h_fig2 ]
    ; [ lit_j_fig2; lit_i_fig2; lit_f_fig2; lit_h_fig2; lit_g_fig2 ]
    ]
    cl_fig2
    tyenv_fig2
    Binding.(from_list [ Free; Bound ])
;;

(** ff *)
let clause_orderings_ff_fig_2 () =
  mk_ordering
    "Paths compatible with {f,f} for Figure 2. from paper"
    []
    cl_fig2
    tyenv_fig2
    Binding.(from_list [ Free; Free ])
;;

(** -- Stuck construction ------------------------------------------------------

f(X) :- h{+}(Y), i{+}(X).

----------------------------------------------------------------------------- *)
let cl_stuck =
  Clause.Raw.clause
    Lit.Raw.(lit pred_f Term.[ var "X" ])
    Lit.Raw.[ lit pred_h Term.[ var "Y" ]; lit pred_i Term.[ var "X" ] ]
;;

let tyenv_stuck =
  TypingEnv.(
    add_pred ~name:name_i ~info:info_i
    @@ add_pred ~name:name_h ~info:info_h
    @@ empty)
;;

let stuck_constraint () =
  mk_constraint
    "Clause constraint for stuck schedule graph"
    Constraint.ill
    cl_stuck
    tyenv_stuck
;;

let clause_orderings_stuck () =
  mk_ordering
    "Paths compatible with {b} for stuck schedule graph"
    []
    cl_stuck
    tyenv_stuck
    Binding.(from_list [ Bound ])
;;

(** -- Wildcard, required ------------------------------------------------------

f(X) :- i(X), h{+}(_).

----------------------------------------------------------------------------- *)
let cl_wildcard_req =
  Clause.Raw.clause
    Lit.Raw.(lit pred_f Term.[ var "X" ])
    Lit.Raw.[ lit_j_fig2; lit pred_h Term.[ wild () ] ]
;;

let tyenv_wildcard_req = TypingEnv.(add_pred ~name:name_h ~info:info_h empty)

let wildcard_req_constraint () =
  mk_constraint
    "Clause constraint for schedule graph stuck on required wilcard"
    Constraint.ill
    cl_wildcard_req
    tyenv_wildcard_req
;;

(** -- Wildcard, optional ------------------------------------------------------

f(X) :- i(X), h(_).

----------------------------------------------------------------------------- *)

let tyenv_wildcard_opt = TypingEnv.empty

let wildcard_opt_constraint () =
  mk_constraint
    "Clause constraint for schedule graph with optional wilcard"
    Constraint.trivial
    cl_wildcard_req
    tyenv_wildcard_opt
;;

(** -- Negation, schedulable ---------------------------------------------------

r(Y,X) :- f{+}(?X), not g(?X,?Y,?Z), h(?Z)

Negated literals must be fully bound; this case is schedulable since `X` occurs
in the head of the clause and `Z` is not required in `h(Z)`.

----------------------------------------------------------------------------- *)
let cl_neg =
  Clause.Raw.clause
    Lit.Raw.(lit pred_r Term.[ var "Y"; var "X" ])
    [ lit_f_fig2; Lit.Raw.neg lit_g_fig2; lit_h_fig2 ]
;;

let tyenv_neg_ok = TypingEnv.(add_pred ~name:name_f ~info:info_f empty)

let neg_ok_constraint () =
  mk_constraint
    "Negated literal, schedulable"
    Constraint.(of_list Atomic.[ of_list [ 0; 1 ] ])
    cl_neg
    tyenv_neg_ok
;;

(** -- Negation, unschedulable -------------------------------------------------

r(Y,X) :- f{+}(?X), not g(?X,?Y,?Z), h{+}(?Z)

This case is unschedulable since `Z` is required in both `g(X,Y,Z)` and `h(Z)`
and does not occur in the head of the calsue.
----------------------------------------------------------------------------- *)

let tyenv_neg_bad =
  TypingEnv.(
    add_pred ~name:name_f ~info:info_f
    @@ add_pred ~name:name_h ~info:info_h
    @@ empty)
;;

let neg_bad_constraint () =
  mk_constraint
    "Negated literal, unschedulable"
    Constraint.ill
    cl_neg
    tyenv_neg_bad
;;

(** -- Negation, constraints overriden, unschedulable --------------------------

r(Y,X) :- f{+}(?X), not g{++?,+?+}(?X,?Y,?Z), h{+}(?Z)

----------------------------------------------------------------------------- *)

let tyenv_neg_bad_override =
  TypingEnv.(
    add_pred ~name:name_g ~info:info_g
    @@ add_pred ~name:name_f ~info:info_f
    @@ add_pred ~name:name_h ~info:info_h
    @@ empty)
;;

let neg_bad_override_constraint () =
  mk_constraint
    "Negated literal, constraint overriden, unschedulable"
    Constraint.ill
    cl_neg
    tyenv_neg_bad_override
;;

(** -- Effectful, schedulable --------------------------------------------------


r(Y,X) :- n{+?}<RW>(Z,Y), m{+?}<HTTP>(X,Z).

Without effects, we can simply move m in front of n so `Z` becomes bound. With
effects we have to maintain the relative order of extrapred predicates
where there effects sets intersect.

Here we can still move `m` ahead of `n` since  they have non-intersecting 
effects.

----------------------------------------------------------------------------- *)

let name_m = Name.from_string "m"
let name_n = Name.from_string "n"
let name_o = Name.from_string "o"
let pred_m = Pred.(pred ~arity:2 name_m)
let pred_n = Pred.(pred ~arity:2 name_n)
let pred_o = Pred.(pred ~arity:2 name_o)

let info_m =
  TypingEnv.
    { cstr = Constraint.(of_list Atomic.[ of_list [ 0 ] ])
    ; nature = Extralogical Eff.[ EffHTTP ]
    ; typing = Typing.bottom
    }
;;

let info_n =
  TypingEnv.
    { cstr = Constraint.(of_list Atomic.[ of_list [ 0 ] ])
    ; nature = Extralogical Eff.[ EffRW ]
    ; typing = Typing.bottom
    }
;;

let info_o =
  TypingEnv.
    { cstr = Constraint.(of_list Atomic.[ of_list [ 0 ] ])
    ; nature = Extralogical Eff.[ EffRW; EffHTTP ]
    ; typing = Typing.bottom
    }
;;

let cl_eff_ok =
  Clause.Raw.clause
    Lit.Raw.(lit pred_r Term.[ var "Y"; var "X" ])
    Lit.Raw.
      [ lit pred_n Term.[ var "Z"; var "Y" ]
      ; lit pred_m Term.[ var "X"; var "Z" ]
      ]
;;

let tyenv_eff =
  TypingEnv.(
    add_pred ~name:name_m ~info:info_m
    @@ add_pred ~name:name_n ~info:info_n
    @@ add_pred ~name:name_o ~info:info_o
    @@ empty)
;;

let eff_ok_constraint () =
  mk_constraint
    "Effectful literals, non-intersecting effects, schedulable"
    Constraint.(of_list Atomic.[ of_list [ 1 ] ])
    cl_eff_ok
    tyenv_eff
;;

(** -- Effectful, unschedulable ------------------------------------------------


r(Y,X) :- n{+?}<RW>(Z,Y), o{+?}<RW,HTTP>(X,Z).

Here we can't still move `o` ahead of `n` since  they have intersecting 
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

let eff_bad_constraint () =
  mk_constraint
    "Effectful literals, intersecting effects, unschedulable"
    Constraint.ill
    cl_eff_bad
    tyenv_eff
;;

(* -------------------------------------------------------------------------- *)

(** Simple negation *)
let cl_simple_neg =
  Clause.Raw.clause
    Lit.Raw.(lit pred_r Term.[ var "X"; var "Y" ])
    Lit.Raw.[ lit ~pol:Neg pred_j Term.[ var "X"; var "Y" ] ]
;;

let tyenv_simple_neg = TypingEnv.empty

let simple_neg_constraint () =
  mk_constraint
    "Simple negation, schedulable"
    Constraint.(of_list Atomic.[ of_list [ 0; 1 ] ])
    cl_simple_neg
    tyenv_simple_neg
;;

(* -------------------------------------------------------------------------- *)

(** Recursion  *)

let cl_recursion =
  Clause.Raw.clause
    Lit.Raw.(lit pred_r Term.[ var "X"; var "Y" ])
    Lit.Raw.
      [ lit pred_j Term.[ var "X"; var "Z" ]
      ; lit pred_r Term.[ var "Z"; var "Y" ]
      ]
;;

let tyenv_recursion = TypingEnv.empty

let recursion_constraint () =
  mk_constraint
    "Recursion, schedulable"
    Constraint.trivial
    cl_recursion
    tyenv_recursion
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Simple negation" `Quick simple_neg_constraint
    ; test_case "Recursion" `Quick recursion_constraint
    ; test_case "Clause constraint for Fig 2" `Quick clause_constraint_fig2
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
    ]
;;
