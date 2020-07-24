open Core_kernel
open Core

module Env = struct
  type t = Ty.TRG.t
end

module M = struct
  include Effect.MonadReader.Make_with_env (Env)

  let subtypes_of ty =
    map ask ~f:(fun trg ->
        Option.value ~default:Ty.Set.empty @@ Ty.TRG.subtypes_of trg ~ty)
  ;;
end

module TTCM = TTC.Make (M)

(* -- Examples TTCs --------------------------------------------------------- *)
let ttc_top2 = TTC.top 2

let ttc_bot2_eq =
  TTC.ttc [ Bot; Bot ] ~equiv:Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
;;

let ttc_a = TTC.ttc [ Symbol; Symbol ]
let ttc_b = TTC.ttc Programs.BikeShop.[ ty_bicycle; ty_wheel ]
let ttc_a_b = TTC.ttc Programs.BikeShop.[ Symbol; Symbol; ty_bicycle; ty_wheel ]

let ttc_a_eq =
  TTC.ttc
    [ Symbol; Symbol ]
    ~equiv:Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
;;

let ttc_b_eq =
  TTC.ttc
    Programs.BikeShop.[ ty_bicycle; ty_wheel ]
    ~equiv:Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
;;

let ttc_a_b_eq =
  TTC.ttc
    Programs.BikeShop.[ Symbol; Symbol; ty_bicycle; ty_wheel ]
    ~equiv:Partition.(of_list Int.Set.[ of_list [ 0; 1 ]; of_list [ 2; 3 ] ])
;;

(* -- Meet ------------------------------------------------------------------ *)

let mk_ttc_meet trg expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") TTC.pp
      @@ pair ~sep:(any "@;/\\@;") TTC.pp TTC.pp)
      (expect, (lhs, rhs))
  in
  let f () =
    Alcotest.check Testable.ttc msg expect M.(run ~env:trg @@ TTCM.meet lhs rhs)
  in
  Alcotest.test_case msg `Quick f
;;

(* -- Cartesian product ----------------------------------------------------- *)

let mk_ttc_product expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") TTC.pp
      @@ pair ~sep:(any "@;*@;") TTC.pp TTC.pp)
      (expect, (lhs, rhs))
  in
  let f () = Alcotest.check Testable.ttc msg expect TTC.(product lhs rhs) in
  Alcotest.test_case msg `Quick f
;;

(* -- Projection ------------------------------------------------------------ *)

let mk_ttc_project expect flds rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") TTC.pp
      @@ pair (prefix (any "pi") @@ braces @@ list ~sep:comma int)
      @@ parens TTC.pp)
      (expect, (flds, rhs))
  in
  let f () = Alcotest.check Testable.ttc msg expect TTC.(project rhs ~flds) in
  Alcotest.test_case msg `Quick f
;;

let ttc_meet_within () =
  Alcotest.(check @@ list Testable.ty)
    "Meet helper"
    Ty.[ bottom; bottom ]
    (M.run ~env:Programs.BikeShop.closure
    @@ TTCM.meet_within
         Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
         Programs.BikeShop.[ ty_bicycle; ty_wheel ])
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "meet within equivalence classes" `Quick ttc_meet_within
    ; mk_ttc_meet Programs.BikeShop.closure ttc_b ttc_top2 ttc_b
    ; mk_ttc_meet Programs.BikeShop.closure ttc_a ttc_top2 ttc_a
    ; mk_ttc_meet Programs.BikeShop.closure ttc_b ttc_a ttc_b
    ; mk_ttc_meet Programs.BikeShop.closure ttc_bot2_eq ttc_a_eq ttc_b
    ; mk_ttc_meet Programs.BikeShop.closure ttc_bot2_eq ttc_a_eq ttc_b_eq
    ; mk_ttc_product ttc_a_b ttc_a ttc_b
    ; mk_ttc_product ttc_a_b_eq ttc_a_eq ttc_b_eq
    ; mk_ttc_project ttc_a [ 0; 1 ] ttc_a_b
    ; mk_ttc_project ttc_b [ 2; 3 ] ttc_a_b
    ; mk_ttc_project ttc_a_eq [ 0; 1 ] ttc_a_b_eq
    ; mk_ttc_project ttc_b_eq [ 2; 3 ] ttc_a_b_eq
    ]
;;
