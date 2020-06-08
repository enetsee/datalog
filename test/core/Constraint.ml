open Core

let testable_constraint = Constraint.(Alcotest.testable pp equal)
let c1 = Constraint.(of_list Atomic.[ of_list [ 1; 2 ] ])
let c2 = Constraint.(of_list Atomic.[ of_list [ 1 ] ])
let c3 = Constraint.(of_list Atomic.[ of_list [ 2 ] ])
let c4 = Constraint.(of_list Atomic.[ of_list [ 1 ]; of_list [ 2 ] ])
let c5 = Constraint.(of_list Atomic.[ of_list [ 1; 2 ]; of_list [ 2; 3 ] ])
let c6 = Constraint.(of_list Atomic.[ of_list [ 3 ] ])
let c7 = Constraint.(of_list Atomic.[ of_list [ 1; 2; 3 ] ])

(* -- Constraint join ------------------------------------------------------- *)

let join_superset () =
  Alcotest.(check testable_constraint)
    "Join with superset"
    c2
    Constraint.(join c1 c2)
;;

let join_no_superset () =
  Alcotest.(check testable_constraint)
    "Join with no superset"
    c4
    Constraint.(join c2 c3)
;;

(* -- TODO: replace with property based tests -- *)

let join_left_identity () =
  Alcotest.(check testable_constraint)
    "Ill-constraint is left identity of join"
    c4
    Constraint.(join ill c4)
;;

let join_right_identity () =
  Alcotest.(check testable_constraint)
    "Ill-constraint is right identity  of join"
    c1
    Constraint.(join c1 ill)
;;

let join_assoc () =
  Alcotest.(check testable_constraint)
    "Join is associative"
    Constraint.(join c1 (join c2 c3))
    Constraint.(join (join c1 c2) c3)
;;

let join_many () =
  Alcotest.(check testable_constraint)
    "Join is associative"
    Constraint.(join c1 (join c2 c3))
    Constraint.(join_list [ c1; c2; c3 ])
;;

(* -- Constraint meet ------------------------------------------------------- *)
let meet_superset () =
  Alcotest.(check testable_constraint)
    "Meet with superset"
    c1
    Constraint.(meet c2 c5)
;;

let meet_no_superset () =
  Alcotest.(check testable_constraint)
    "Meet with no superset"
    c7
    Constraint.(meet c1 c6)
;;

let meet_left_identity () =
  Alcotest.(check testable_constraint)
    "Trivial-constraint is left identity of meet"
    c5
    Constraint.(meet trivial c5)
;;

let meet_right_identity () =
  Alcotest.(check testable_constraint)
    "Trivial-constraint is right identity of meet"
    c5
    Constraint.(meet c5 trivial)
;;

let meet_assoc () =
  Alcotest.(check testable_constraint)
    "Meet is associative"
    Constraint.(join c2 (join c5 c6))
    Constraint.(join (join c2 c5) c6)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Join with superset" `Quick join_superset
    ; test_case "Join with no superset" `Quick join_no_superset
    ; test_case
        "Join has ill-constraint as left identity"
        `Quick
        join_left_identity
    ; test_case
        "Join has ill-constraint as right identity"
        `Quick
        join_right_identity
    ; test_case "Join is associative" `Quick join_assoc
    ; test_case "Join list is the same as manual joins" `Quick join_many
    ; test_case "Meet with superset" `Quick meet_superset
    ; test_case "Meet with no superset" `Quick meet_no_superset
    ; test_case
        "Trivial-constraint is left identity of meet"
        `Quick
        meet_left_identity
    ; test_case
        "Trivial-constraint is right identity of meet"
        `Quick
        meet_right_identity
    ; test_case "Meet is associative" `Quick meet_assoc
    ]
;;
