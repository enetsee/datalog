open Core
open Programs

module Env = struct
  type t = Ty.TRG.t
end

module M = struct
  include Effect.MonadReader.Make (Env)

  let subtypes_of ty =
    map ask ~f:(fun trg ->
        Option.value ~default:Ty.Set.empty @@ Ty.TRG.subtypes_of trg ~ty)
  ;;
end

module TyM = Ty.Make (M)

let ty_transitive_closure_example () =
  Alcotest.check
    Testable.trg
    "Bike shop example"
    Stratified.BikeShop.closure
    Ty.TRG.(from_list Stratified.BikeShop.subtys)
;;

let mk_ty_meet trg expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Ty.pp
      @@ pair ~sep:(any "@;/\\@;") Ty.pp Ty.pp)
      (expect, (lhs, rhs))
  in
  let f () =
    Alcotest.check Testable.ty msg expect M.(run (TyM.meet lhs rhs) ~env:trg)
  in
  Alcotest.test_case msg `Quick f
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case
        "Bike shope example: transitive closure of subtype relationships"
        `Quick
        ty_transitive_closure_example
      (* TODO: replace with property based test: forall ty, Top /\\ ty === ty *)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Top Ty.Top Ty.Top)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Symbol Ty.Top Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Number Ty.Top Ty.Number)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Real Ty.Top Ty.Real)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Int Ty.Top Ty.Int)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Bool Ty.Top Ty.Bool)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_cycle Ty.Top ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_bicycle Ty.Top ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_unicycle Ty.Top ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_saddle Ty.Top ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_pedals Ty.Top ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_wheel Ty.Top ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_spokes Ty.Top ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_tire Ty.Top ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_tube Ty.Top ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Symbol Ty.Symbol Ty.Top)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Symbol Ty.Symbol Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Symbol Ty.Number)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Symbol Ty.Real)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Symbol Ty.Int)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Symbol Ty.Bool)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_cycle Ty.Symbol ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_bicycle Ty.Symbol ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_unicycle Ty.Symbol ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_saddle Ty.Symbol ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_pedals Ty.Symbol ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_wheel Ty.Symbol ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_spokes Ty.Symbol ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_tire Ty.Symbol ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_tube Ty.Symbol ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Number Ty.Number Ty.Top)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Number Ty.Number Ty.Number)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Real Ty.Number Ty.Real)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Int Ty.Number Ty.Int)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number Ty.Bool)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Number ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Real Ty.Real Ty.Top)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Real Ty.Real Ty.Number)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Real Ty.Real Ty.Real)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real Ty.Int)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real Ty.Bool)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Real ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Int Ty.Int Ty.Top)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Int Ty.Int Ty.Number)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Int Ty.Int Ty.Int)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int Ty.Real)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int Ty.Bool)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Int ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.Bool Ty.Bool Ty.Top)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool Ty.Number)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool Ty.Real)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool Ty.Int)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom Ty.Bool ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_cycle ty_cycle Ty.top)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_cycle ty_cycle Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_cycle ty_cycle ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_bicycle ty_cycle ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_unicycle ty_cycle ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_cycle ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_cycle ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_cycle ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_cycle ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_cycle ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_cycle ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle Ty.top)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_bicycle ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_bicycle ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_bicycle ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_bicycle ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_bicycle ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_bicycle ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_bicycle ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_unicycle ty_unicycle Ty.top)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_unicycle ty_unicycle Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_unicycle ty_unicycle ty_cycle)
    ; Stratified.BikeShop.(
        mk_ty_meet closure ty_unicycle ty_unicycle ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_unicycle ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_unicycle ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_unicycle ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_unicycle ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_unicycle ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_unicycle ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_unicycle ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_saddle ty_saddle Ty.top)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_saddle ty_saddle Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_saddle ty_saddle ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_saddle ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_saddle ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_saddle ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_saddle ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_saddle ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_saddle ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_saddle ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_saddle ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_pedals ty_pedals Ty.top)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_pedals ty_pedals Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_pedals ty_pedals ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_pedals ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_pedals ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_pedals ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_pedals ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_pedals ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_pedals ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_pedals ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_pedals ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_wheel ty_wheel Ty.top)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_wheel ty_wheel Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_wheel ty_wheel ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_wheel ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_wheel ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_wheel ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_wheel ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_wheel ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_wheel ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_wheel ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_wheel ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_spokes ty_spokes Ty.top)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_spokes ty_spokes Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_spokes ty_spokes ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_spokes ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_spokes ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_spokes ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_spokes ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_spokes ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_spokes ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_spokes ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_spokes ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_tire ty_tire Ty.top)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_tire ty_tire Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_tire ty_tire ty_tire)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tire ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tire ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tire ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tire ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tire ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tire ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tire ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tire ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_tube ty_tube Ty.top)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_tube ty_tube Ty.Symbol)
    ; Stratified.BikeShop.(mk_ty_meet closure ty_tube ty_tube ty_tube)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tube ty_cycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tube ty_bicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tube ty_unicycle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tube ty_saddle)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tube ty_pedals)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tube ty_wheel)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tube ty_spokes)
    ; Stratified.BikeShop.(mk_ty_meet closure Ty.bottom ty_tube ty_tire)
    ]
;;
