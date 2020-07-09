let relation_of_hasPartTC () =
  Alcotest.check
    Testable.relation
    "Relation for recursive clause"
    Programs.BikeShop.rel_hasPartTC
    Core.Relation.(of_clauses Programs.BikeShop.stratum2)
;;

let relation_of_socins () =
  Alcotest.check
    Testable.relation
    "Relation with variable reordering"
    Programs.SocialInsurance.rel_socins
    Core.Relation.(of_clauses Programs.SocialInsurance.stratum3)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Relation for recursive clause" `Quick relation_of_hasPartTC
    ; test_case "Relation with variable reordering" `Quick relation_of_socins
    ]
;;
