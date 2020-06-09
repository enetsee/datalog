open Core

let testable_constraint = Constraint.(Alcotest.testable pp equal)
let testable_schedule = Schedule.(Alcotest.testable pp equal)
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

let cl_figure2 =
  Raw.(
    Clause.clause
      Lit.(lit pred_r Term.[ var "Y"; var "Z" ])
      Lit.
        [ lit pred_f Term.[ var "X" ]
        ; lit pred_g Term.[ var "X"; var "Y"; var "Z" ]
        ; lit pred_h Term.[ var "Z" ]
        ; lit pred_i Term.[ var "X" ]
        ; lit pred_j Term.[ var "X"; var "W" ]
        ])
;;

let cstrs_figure2 =
  Pred.Map.of_alist_exn [ pred_f, cstr_f; pred_g, cstr_g; pred_h, cstr_h ]
;;

let sched_figure2 = Schedule.of_clause cl_figure2 ~cstrs:cstrs_figure2

let figure2 () =
  Alcotest.(check testable_constraint)
    "Unfixable on one path, fixable on other."
    Constraint.(of_list Atomic.[ of_list [ 0 ]; of_list [ 1 ] ])
    Schedule.(extract sched_figure2)
;;

(** -- Example 4.9, stuck construction -----------------------------------------

f(X) :- h{+}(Y), i{+}(X).

----------------------------------------------------------------------------- *)
let cl_example_4_9 =
  Raw.(
    Clause.clause
      Lit.(lit pred_f Term.[ var "X" ])
      Lit.[ lit pred_h Term.[ var "Y" ]; lit pred_i Term.[ var "X" ] ])
;;

let cstrs_example_4_9 = Pred.Map.of_alist_exn [ pred_i, cstr_i; pred_h, cstr_h ]
let sched_figure2 = Schedule.of_clause cl_example_4_9 ~cstrs:cstrs_example_4_9

let example_4_9 () =
  Alcotest.(check testable_constraint)
    "Unfixable on one path, fixable on other."
    Constraint.ill
    Schedule.(extract sched_figure2)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Example from figure 2 of paper" `Quick figure2
    ; test_case "Example 4.9 of paper" `Quick example_4_9
    ]
;;
