open Core_kernel
open Core
open Programs.Stratified

let rel_has_part_tc () =
  Alcotest.check
    Testable.relation
    "Recursive transitive closure definition"
    Relation.(
      union (pred BikeShop.pr_hasPart)
      @@ project ~flds:Int.Set.(of_list [ 0; 3 ])
      @@ restrict ~equiv:(1, 2)
      @@ product (pred BikeShop.pr_hasPartTC) (pred BikeShop.pr_hasPart))
    Relation.(of_clauses BikeShop.stratum2)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Recursive transitive closure definition" `Quick rel_has_part_tc
    ]
;;
