let test_suite =
  [ "Dataflow", Dataflow.test_cases
  ; "Dead clause elimination", DeadClause.test_cases
  ; "Range restriction repair", RangeRestrict.test_cases
  ; "Constraint", Constraint.test_cases
  ; "Subgoal scheduling", Schedule.test_cases
  ]
;;
