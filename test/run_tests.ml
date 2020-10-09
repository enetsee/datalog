let test_suites : unit Alcotest.test list =
  [ "adornment", Test_adorn.test_cases
  ; "dataflow", Test_dataflow.test_cases
  ; "dependency", Test_dependency.test_cases
  ]
;;

let () = Alcotest.run "proj" test_suites
