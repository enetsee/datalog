open Test_common

module Env = struct
  type t = Type.TRG.t
end

module M = struct
  include Effect.MonadReader.Make_with_env (Env)

  let subtypes_of ty =
    map ask ~f:(fun trg ->
        Option.value ~default:Type.Ty.Set.empty @@ Type.TRG.subtypes_of trg ~ty)
  ;;
end

module TyM = Typecheck.Ty.Make (M)

let ty_transitive_closure_example () =
  Alcotest.check
    Testable.trg
    "Bike shop example"
    Programs.BikeShop.closure
    (Type.TRG.from_list Programs.BikeShop.subtys)
;;

let mk_ty_meet trg expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Type.Ty.pp
      @@ pair ~sep:(any "@;/\\@;") Type.Ty.pp Type.Ty.pp)
      (expect, (lhs, rhs))
  in
  let f () =
    Alcotest.check Testable.ty msg expect M.(run (TyM.meet lhs rhs) ~env:trg)
  in
  Alcotest.test_case msg `Quick f
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  [ Alcotest.test_case
      "Bike shope example: transitive closure of subtype relationships"
      `Quick
      ty_transitive_closure_example
    (* TODO: replace with property based test: forall ty, Top /\\ ty === ty *)
  ; mk_ty_meet Programs.BikeShop.closure Type.Ty.Top Type.Ty.Top Type.Ty.Top
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Symbol Type.Ty.Top Type.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Number Type.Ty.Top Type.Ty.Number)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Real Type.Ty.Top Type.Ty.Real)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Int Type.Ty.Top Type.Ty.Int)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bool Type.Ty.Top Type.Ty.Bool)
  ; Programs.BikeShop.(mk_ty_meet closure ty_cycle Type.Ty.Top ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle Type.Ty.Top ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_unicycle Type.Ty.Top ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_saddle Type.Ty.Top ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_pedals Type.Ty.Top ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure ty_wheel Type.Ty.Top ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure ty_spokes Type.Ty.Top ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tire Type.Ty.Top ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tube Type.Ty.Top ty_tube)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Symbol Type.Ty.Symbol Type.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Symbol Type.Ty.Symbol Type.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Symbol Type.Ty.Number)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Symbol Type.Ty.Real)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Symbol Type.Ty.Int)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Symbol Type.Ty.Bool)
  ; Programs.BikeShop.(mk_ty_meet closure ty_cycle Type.Ty.Symbol ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle Type.Ty.Symbol ty_bicycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure ty_unicycle Type.Ty.Symbol ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_saddle Type.Ty.Symbol ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_pedals Type.Ty.Symbol ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure ty_wheel Type.Ty.Symbol ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure ty_spokes Type.Ty.Symbol ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tire Type.Ty.Symbol ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tube Type.Ty.Symbol ty_tube)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Number Type.Ty.Number Type.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Number Type.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Number Type.Ty.Number Type.Ty.Number)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Real Type.Ty.Number Type.Ty.Real)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Int Type.Ty.Number Type.Ty.Int)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Number Type.Ty.Bool)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Number ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Number ty_bicycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Number ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Number ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Number ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Number ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Number ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Number ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Number ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Real Type.Ty.Real Type.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Real Type.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Real Type.Ty.Real Type.Ty.Number)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Real Type.Ty.Real Type.Ty.Real)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real Type.Ty.Int)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real Type.Ty.Bool)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Real ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Int Type.Ty.Int Type.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Int Type.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Int Type.Ty.Int Type.Ty.Number)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Int Type.Ty.Int Type.Ty.Int)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int Type.Ty.Real)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int Type.Ty.Bool)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Int ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bool Type.Ty.Bool Type.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool Type.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool Type.Ty.Number)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool Type.Ty.Real)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool Type.Ty.Int)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot Type.Ty.Bool ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_cycle ty_cycle Type.Ty.Top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_cycle ty_cycle Type.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_cycle ty_cycle ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle ty_cycle ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_unicycle ty_cycle ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_cycle ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_cycle ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_cycle ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_cycle ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_cycle ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_cycle ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle Type.Ty.Top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle Type.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_bicycle ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_bicycle ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_bicycle ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_bicycle ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_bicycle ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_bicycle ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_bicycle ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_unicycle ty_unicycle Type.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure ty_unicycle ty_unicycle Type.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_unicycle ty_unicycle ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_unicycle ty_unicycle ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_unicycle ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_unicycle ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_unicycle ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_unicycle ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_unicycle ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_unicycle ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_unicycle ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_saddle ty_saddle Type.Ty.Top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_saddle ty_saddle Type.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_saddle ty_saddle ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_saddle ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_saddle ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_saddle ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_saddle ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_saddle ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_saddle ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_saddle ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_saddle ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_pedals ty_pedals Type.Ty.Top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_pedals ty_pedals Type.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_pedals ty_pedals ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_pedals ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_pedals ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_pedals ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_pedals ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_pedals ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_pedals ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_pedals ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_pedals ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_wheel ty_wheel Type.Ty.Top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_wheel ty_wheel Type.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_wheel ty_wheel ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_wheel ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_wheel ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_wheel ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_wheel ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_wheel ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_wheel ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_wheel ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_wheel ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_spokes ty_spokes Type.Ty.Top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_spokes ty_spokes Type.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_spokes ty_spokes ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_spokes ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_spokes ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_spokes ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_spokes ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_spokes ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_spokes ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_spokes ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_spokes ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tire ty_tire Type.Ty.Top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tire ty_tire Type.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tire ty_tire ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tire ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tire ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tire ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tire ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tire ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tire ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tire ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tire ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tube ty_tube Type.Ty.Top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tube ty_tube Type.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tube ty_tube ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tube ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tube ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tube ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tube ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tube ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tube ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tube ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Type.Ty.Bot ty_tube ty_tire)
  ]
;;
