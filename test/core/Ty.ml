module Env = struct
  type t = Core.Ty.TRG.t
end

module M = struct
  include Effect.MonadReader.Make_with_env (Env)

  let subtypes_of ty =
    map ask ~f:(fun trg ->
        Option.value ~default:Core.Ty.Set.empty
        @@ Core.Ty.TRG.subtypes_of trg ~ty)
  ;;
end

module TyM = Core.Ty.Make (M)

let ty_transitive_closure_example () =
  Alcotest.check
    Testable.trg
    "Bike shop example"
    Programs.BikeShop.closure
    (Core.Ty.TRG.from_list Programs.BikeShop.subtys)
;;

let mk_ty_meet trg expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Core.Ty.pp
      @@ pair ~sep:(any "@;/\\@;") Core.Ty.pp Core.Ty.pp)
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
  ; mk_ty_meet Programs.BikeShop.closure Core.Ty.Top Core.Ty.Top Core.Ty.Top
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Symbol Core.Ty.Top Core.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Number Core.Ty.Top Core.Ty.Number)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.Real Core.Ty.Top Core.Ty.Real)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.Int Core.Ty.Top Core.Ty.Int)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.Bool Core.Ty.Top Core.Ty.Bool)
  ; Programs.BikeShop.(mk_ty_meet closure ty_cycle Core.Ty.Top ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle Core.Ty.Top ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_unicycle Core.Ty.Top ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_saddle Core.Ty.Top ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_pedals Core.Ty.Top ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure ty_wheel Core.Ty.Top ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure ty_spokes Core.Ty.Top ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tire Core.Ty.Top ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tube Core.Ty.Top ty_tube)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Symbol Core.Ty.Symbol Core.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Symbol Core.Ty.Symbol Core.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Symbol Core.Ty.Number)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Symbol Core.Ty.Real)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Symbol Core.Ty.Int)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Symbol Core.Ty.Bool)
  ; Programs.BikeShop.(mk_ty_meet closure ty_cycle Core.Ty.Symbol ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle Core.Ty.Symbol ty_bicycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure ty_unicycle Core.Ty.Symbol ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_saddle Core.Ty.Symbol ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_pedals Core.Ty.Symbol ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure ty_wheel Core.Ty.Symbol ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure ty_spokes Core.Ty.Symbol ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tire Core.Ty.Symbol ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tube Core.Ty.Symbol ty_tube)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Number Core.Ty.Number Core.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Number Core.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Number Core.Ty.Number Core.Ty.Number)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Real Core.Ty.Number Core.Ty.Real)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Int Core.Ty.Number Core.Ty.Int)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Number Core.Ty.Bool)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Number ty_cycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Number ty_bicycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Number ty_unicycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Number ty_saddle)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Number ty_pedals)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Number ty_wheel)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Number ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Number ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Number ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.Real Core.Ty.Real Core.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Real Core.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Real Core.Ty.Real Core.Ty.Number)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Real Core.Ty.Real Core.Ty.Real)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Real Core.Ty.Int)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Real Core.Ty.Bool)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Real ty_cycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Real ty_bicycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Real ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Real ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Real ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Real ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Real ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Real ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Real ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.Int Core.Ty.Int Core.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Int Core.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.Int Core.Ty.Int Core.Ty.Number)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.Int Core.Ty.Int Core.Ty.Int)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Int Core.Ty.Real)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Int Core.Ty.Bool)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Int ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Int ty_bicycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Int ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Int ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Int ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Int ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Int ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Int ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Int ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.Bool Core.Ty.Bool Core.Ty.Top)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool Core.Ty.Symbol)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool Core.Ty.Number)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool Core.Ty.Real)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool Core.Ty.Int)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool ty_cycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool ty_bicycle)
  ; Programs.BikeShop.(
      mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom Core.Ty.Bool ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_cycle ty_cycle Core.Ty.top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_cycle ty_cycle Core.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_cycle ty_cycle ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle ty_cycle ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_unicycle ty_cycle ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_cycle ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_cycle ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_cycle ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_cycle ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_cycle ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_cycle ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle Core.Ty.top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle Core.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_bicycle ty_bicycle ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_bicycle ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_bicycle ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_bicycle ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_bicycle ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_bicycle ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_bicycle ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_bicycle ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_unicycle ty_unicycle Core.Ty.top)
  ; Programs.BikeShop.(
      mk_ty_meet closure ty_unicycle ty_unicycle Core.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_unicycle ty_unicycle ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure ty_unicycle ty_unicycle ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_unicycle ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_unicycle ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_unicycle ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_unicycle ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_unicycle ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_unicycle ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_unicycle ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_saddle ty_saddle Core.Ty.top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_saddle ty_saddle Core.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_saddle ty_saddle ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_saddle ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_saddle ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_saddle ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_saddle ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_saddle ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_saddle ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_saddle ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_saddle ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_pedals ty_pedals Core.Ty.top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_pedals ty_pedals Core.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_pedals ty_pedals ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_pedals ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_pedals ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_pedals ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_pedals ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_pedals ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_pedals ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_pedals ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_pedals ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_wheel ty_wheel Core.Ty.top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_wheel ty_wheel Core.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_wheel ty_wheel ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_wheel ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_wheel ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_wheel ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_wheel ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_wheel ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_wheel ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_wheel ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_wheel ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_spokes ty_spokes Core.Ty.top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_spokes ty_spokes Core.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_spokes ty_spokes ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_spokes ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_spokes ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_spokes ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_spokes ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_spokes ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_spokes ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_spokes ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_spokes ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tire ty_tire Core.Ty.top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tire ty_tire Core.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tire ty_tire ty_tire)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tire ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tire ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tire ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tire ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tire ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tire ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tire ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tire ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tube ty_tube Core.Ty.top)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tube ty_tube Core.Ty.Symbol)
  ; Programs.BikeShop.(mk_ty_meet closure ty_tube ty_tube ty_tube)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tube ty_cycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tube ty_bicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tube ty_unicycle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tube ty_saddle)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tube ty_pedals)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tube ty_wheel)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tube ty_spokes)
  ; Programs.BikeShop.(mk_ty_meet closure Core.Ty.bottom ty_tube ty_tire)
  ]
;;
