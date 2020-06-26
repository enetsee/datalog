open Core
open Programs.Stratified

let relation_of_hasPartTC () =
  Alcotest.check
    Testable.relation
    "Relation for recursive clause"
    BikeShop.rel_hasPartTC
    Relation.(of_clauses BikeShop.stratum2)
;;

let relation_of_socins () =
  Alcotest.check
    Testable.relation
    "Relation with variable reordering"
    SocialInsurance.rel_socins
    Relation.(of_clauses SocialInsurance.stratum3)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Relation for recursive clause" `Quick relation_of_hasPartTC
    ; test_case "Relation with variable reordering" `Quick relation_of_socins
    ]
;;
