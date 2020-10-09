open Core_kernel
open Test_common

let mk_strata msg expect prog =
  let f () =
    Alcotest.(check @@ result Testable.strata Testable.cycles)
      msg
      expect
      Dependency.Adorned.(stratify @@ from_program prog)
  in
  Alcotest.test_case msg `Quick f
;;

let no_cycles =
  Programs.Alice.(
    mk_strata "Alice example 15.2.5, no cycles" (Ok strata) prg_good_adorned)
;;

let direct_cycles =
  Programs.Alice.(
    mk_strata
      "Alice example Fig 15.1, direct cycle"
      (Error cycles)
      prog_bad_adorned)
;;

let pos_cycle =
  Programs.Comp.(
    mk_strata "Alice example Pc,cmp, direct cycle" (Ok strata) prog_adorned)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases = [ no_cycles; direct_cycles; pos_cycle ]
