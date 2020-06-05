let test_suites : unit Alcotest.test list = List.concat [ Test_core.test_suite ]
let () = Alcotest.run "proj" test_suites
