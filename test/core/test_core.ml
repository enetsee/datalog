let test_suite =
  [ "Dataflow", Dataflow.test_cases
  ; "Dependency", Dependency.test_cases
  ; "Dead clause elimination", DeadClause.test_cases
  ; "Range restriction repair", RangeRestrict.test_cases
  ; "Constraint", Constraint.test_cases
  ; "Subgoal scheduling", Schedule.test_cases
  ; "Generalized adornment", GenAdornment.test_cases
  ; "Stratification", Stratification.test_cases
  ]
;;
