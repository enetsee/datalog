let test_suite =
  [ "Dataflow", Dataflow.test_cases
  ; "Dependency", Dependency.test_cases
  ; "Dead clause elimination", DeadClause.test_cases
  ; "Range restriction repair", RangeRestrict.test_cases
  ; "Constraint", Constraint.test_cases
  ; "Subgoal scheduling", Schedule.test_cases
  ; "Generalized adornment", Adornment.test_cases
  ; "Stratification", Stratification.test_cases
  ; "Partition", Partition.test_cases
  ; "Relation", Relation.test_cases
  ; "Ty", Ty.test_cases
  ; "Type tuple constraint", TTC.test_cases
  ; "Typing", Typing.test_cases
  ]
;;
