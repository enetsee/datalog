open Core_kernel
open Core
open Programs

let mk_meet trg expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Typing.pp
      @@ hovbox
      @@ pair ~sep:(any "@;/\\@;") Typing.pp Typing.pp)
      (expect, (lhs, rhs))
  in
  let f () =
    Alcotest.check Testable.typing msg expect Typing.(meet ~trg lhs rhs)
  in
  Alcotest.test_case msg `Quick f
;;

let mk_join expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Typing.pp
      @@ hovbox
      @@ pair ~sep:(any "@;\\/@;") Typing.pp Typing.pp)
      (expect, (lhs, rhs))
  in
  let f () = Alcotest.check Testable.typing msg expect Typing.(join lhs rhs) in
  Alcotest.test_case msg `Quick f
;;

let mk_product expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Typing.pp
      @@ hovbox
      @@ pair ~sep:(any "@;*@;") Typing.pp Typing.pp)
      (expect, (lhs, rhs))
  in
  let f () =
    Alcotest.check Testable.typing msg expect Typing.(product lhs rhs)
  in
  Alcotest.test_case msg `Quick f
;;

let mk_project expect flds rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Typing.pp
      @@ hovbox
      @@ pair (prefix (any "pi") @@ braces @@ list ~sep:comma int)
      @@ parens Typing.pp)
      (expect, (Int.Set.to_list flds, rhs))
  in
  let f () =
    Alcotest.check Testable.typing msg expect Typing.(project rhs ~flds)
  in
  Alcotest.test_case msg `Quick f
;;

let mk_restrict trg expect cstr rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Typing.pp
      @@ hovbox
      @@ pair (prefix (any "sig") @@ parens @@ pair ~sep:(any "@;~@;") int int)
      @@ parens Typing.pp)
      (expect, (cstr, rhs))
  in
  let f () =
    Alcotest.check Testable.typing msg expect Typing.(restrict rhs ~trg ~cstr)
  in
  Alcotest.test_case msg `Quick f
;;

(*-- Individual steps of fix-point calculation ------------------------------ *)

let rel_hasPartTC = Relation.of_clauses Stratified.BikeShop.stratum2

let typing_hasPartTC_step1 =
  Stratified.BikeShop.(
    Typing.interpret
      rel_hasPartTC
      ~trg:closure
      ~edb:edb1
      ~stratum:Pred.Map.empty)
;;

let typing_of_hasPartTC_step1 () =
  Alcotest.check
    Testable.typing
    "Typing of `hasPart+`, step1"
    Stratified.BikeShop.typing_hasPart
    typing_hasPartTC_step1
;;

let typing_hasPartTC_step2 =
  Stratified.BikeShop.(
    Typing.interpret
      rel_hasPartTC
      ~trg:Stratified.BikeShop.closure
      ~edb:edb1
      ~stratum:
        Pred.Map.(
          of_alist_exn
            Stratified.BikeShop.[ pr_hasPartTC, typing_hasPartTC_step1 ]))
;;

let typing_of_hasPartTC_step2 () =
  Alcotest.check
    Testable.typing
    "Typing of `hasPart+`, step2"
    Stratified.BikeShop.(
      Core.Typing.join typing_hasPart typing_hasPart_2_eq_1_2_proj_0_4)
    typing_hasPartTC_step2
;;

(* -- Stratum typing -------------------------------------------------------- *)

let typing_stratum_1 =
  Pred.Map.of_alist_exn Stratified.BikeShop.[ pr_hasPart, typing_hasPart ]
;;

let typing_of_stratum1 () =
  Alcotest.check
    Testable.(map (module Core.Pred) (module Core.Typing))
    "Typing of stratum 1"
    typing_stratum_1
    Stratified.BikeShop.(Typing.type_of_stratum stratum1 ~trg:closure ~edb:edb0)
;;

let typing_stratum_2 =
  Pred.Map.of_alist_exn Stratified.BikeShop.[ pr_hasPartTC, typing_hasPartTC ]
;;

let typing_of_stratum2 () =
  Alcotest.check
    Testable.(map (module Core.Pred) (module Core.Typing))
    "Typing of stratum 2"
    typing_stratum_2
    Stratified.BikeShop.(Typing.type_of_stratum stratum2 ~trg:closure ~edb:edb1)
;;

let typing_stratum_3 =
  Pred.Map.of_alist_exn Stratified.BikeShop.[ pr_query, typing_query ]
;;

let typing_of_stratum3 () =
  Alcotest.check
    Testable.(map (module Core.Pred) (module Core.Typing))
    "Typing of stratum 3"
    typing_stratum_3
    Stratified.BikeShop.(Typing.type_of_stratum stratum3 ~trg:closure ~edb:edb2)
;;

(* -- Whole program typing -------------------------------------------------- *)

let typing_of_strata () =
  Alcotest.check
    Testable.(map (module Core.Pred) (module Core.Typing))
    "Typing of bike shop program"
    Stratified.BikeShop.edb3
    Stratified.BikeShop.(Typing.type_of strata ~trg:closure ~edb:edb0)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ Stratified.BikeShop.(
        mk_product typing_hasPart_2 typing_hasPart typing_hasPart)
    ; Stratified.BikeShop.(
        mk_restrict closure typing_hasPart_2_eq_1_2 (1, 2) typing_hasPart_2)
    ; Stratified.BikeShop.(
        mk_project
          typing_hasPart_2_eq_1_2_proj_0_4
          Int.Set.(of_list [ 0; 3 ])
          typing_hasPart_2_eq_1_2)
    ; test_case "Typing of `hasPart+`, step1" `Quick typing_of_hasPartTC_step1
    ; test_case "Typing of `hasPart+`, step2" `Quick typing_of_hasPartTC_step2
    ; test_case "Typing of stratum 1" `Quick typing_of_stratum1
    ; test_case "Typing of stratum 2" `Quick typing_of_stratum2
    ; test_case "Typing of stratum 3" `Quick typing_of_stratum3
    ; test_case "Typing of bike shop program" `Quick typing_of_strata
    ]
;;
