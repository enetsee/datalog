open Core_kernel

module Stratified = struct
  module BikeShop = struct
    (* -- Types used & subtyping relationships ---------------------------------- *)
    let ty_cycle = Core.Ty.(named "cycle")
    let ty_bicycle = Core.Ty.(named "bicycle")
    let ty_unicycle = Core.Ty.(named "unicycle")
    let ty_pedals = Core.Ty.(named "pedals")
    let ty_saddle = Core.Ty.(named "saddle")
    let ty_wheel = Core.Ty.(named "wheel")
    let ty_spokes = Core.Ty.(named "spokes")
    let ty_tire = Core.Ty.(named "tire")
    let ty_tube = Core.Ty.(named "tube")

    let subtys =
      Core.Ty.
        [ ty_cycle, Core.Ty.Symbol
        ; ty_bicycle, ty_cycle
        ; ty_unicycle, ty_cycle
        ; ty_pedals, Symbol
        ; ty_saddle, Symbol
        ; ty_wheel, Symbol
        ; ty_spokes, Symbol
        ; ty_tire, Symbol
        ; ty_tube, Symbol
        ]
    ;;

    let closure =
      Core.Ty.Map.of_alist_exn
        Core.Ty.
          [ ( Symbol
            , Set.of_list
                [ Symbol
                ; ty_cycle
                ; ty_bicycle
                ; ty_unicycle
                ; ty_pedals
                ; ty_saddle
                ; ty_wheel
                ; ty_spokes
                ; ty_tire
                ; ty_tube
                ] )
          ; ty_cycle, Set.of_list [ ty_cycle; ty_bicycle; ty_unicycle ]
          ; ty_bicycle, Set.singleton ty_bicycle
          ; ty_unicycle, Set.singleton ty_unicycle
          ; ty_pedals, Set.singleton ty_pedals
          ; ty_saddle, Set.singleton ty_saddle
          ; ty_wheel, Set.singleton ty_wheel
          ; ty_spokes, Set.singleton ty_spokes
          ; ty_tire, Set.singleton ty_tire
          ; ty_tube, Set.singleton ty_tube
          ]
    ;;

    (* -- Predicates ------------------------------------------------------------ *)
    let pr_cycle = Core.Pred.(pred ~arity:1 @@ Name.from_string "cycle")
    let pr_unicycle = Core.Pred.(pred ~arity:4 @@ Name.from_string "unicycle")
    let pr_bicycle = Core.Pred.(pred ~arity:4 @@ Name.from_string "bicycle")
    let pr_wheel = Core.Pred.(pred ~arity:3 @@ Name.from_string "wheel")
    let pr_tire = Core.Pred.(pred ~arity:2 @@ Name.from_string "tire")
    let pr_hasPart = Core.Pred.(pred ~arity:2 @@ Name.from_string "hasPart")
    let pr_hasPartTC = Core.Pred.(pred ~arity:2 @@ Name.from_string "hasPart+")
    let pr_query = Core.Pred.(pred ~arity:2 @@ Name.from_string "query")

    (* -- Typings ----------------------------------------------------------- *)

    let typing_cycle = Core.Typing.of_schema [ ty_cycle ]

    let typing_unicycle =
      Core.Typing.of_schema [ ty_unicycle; ty_saddle; ty_wheel; ty_pedals ]
    ;;

    let typing_bicycle =
      Core.Typing.of_schema [ ty_bicycle; ty_wheel; ty_wheel; ty_pedals ]
    ;;

    let typing_wheel = Core.Typing.of_schema [ ty_wheel; ty_tire; ty_spokes ]
    let typing_tire = Core.Typing.of_schema [ ty_tire; ty_tube ]

    let typing_hasPart =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_bicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_saddle ]
          ; ttc [ ty_unicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_tire ]
          ; ttc [ ty_wheel; ty_spokes ]
          ; ttc [ ty_tire; ty_tube ]
          ]
    ;;

    let typing_hasPart_2 =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_bicycle; ty_pedals; ty_bicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_pedals; ty_bicycle; ty_wheel ]
          ; ttc [ ty_bicycle; ty_pedals; ty_unicycle; ty_saddle ]
          ; ttc [ ty_bicycle; ty_pedals; ty_unicycle; ty_wheel ]
          ; ttc [ ty_bicycle; ty_pedals; ty_unicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_pedals; ty_wheel; ty_tire ]
          ; ttc [ ty_bicycle; ty_pedals; ty_wheel; ty_spokes ]
          ; ttc [ ty_bicycle; ty_pedals; ty_tire; ty_tube ]
          ; ttc [ ty_bicycle; ty_wheel; ty_bicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_wheel; ty_bicycle; ty_wheel ]
          ; ttc [ ty_bicycle; ty_wheel; ty_unicycle; ty_saddle ]
          ; ttc [ ty_bicycle; ty_wheel; ty_unicycle; ty_wheel ]
          ; ttc [ ty_bicycle; ty_wheel; ty_unicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_wheel; ty_wheel; ty_tire ]
          ; ttc [ ty_bicycle; ty_wheel; ty_wheel; ty_spokes ]
          ; ttc [ ty_bicycle; ty_wheel; ty_tire; ty_tube ]
          ; ttc [ ty_unicycle; ty_pedals; ty_bicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_pedals; ty_bicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_pedals; ty_unicycle; ty_saddle ]
          ; ttc [ ty_unicycle; ty_pedals; ty_unicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_pedals; ty_unicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_pedals; ty_wheel; ty_tire ]
          ; ttc [ ty_unicycle; ty_pedals; ty_wheel; ty_spokes ]
          ; ttc [ ty_unicycle; ty_pedals; ty_tire; ty_tube ]
          ; ttc [ ty_unicycle; ty_saddle; ty_bicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_saddle; ty_bicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_saddle; ty_unicycle; ty_saddle ]
          ; ttc [ ty_unicycle; ty_saddle; ty_unicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_saddle; ty_unicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_saddle; ty_wheel; ty_tire ]
          ; ttc [ ty_unicycle; ty_saddle; ty_wheel; ty_spokes ]
          ; ttc [ ty_unicycle; ty_saddle; ty_tire; ty_tube ]
          ; ttc [ ty_unicycle; ty_wheel; ty_bicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_wheel; ty_bicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_wheel; ty_unicycle; ty_saddle ]
          ; ttc [ ty_unicycle; ty_wheel; ty_unicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_wheel; ty_unicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_wheel; ty_wheel; ty_tire ]
          ; ttc [ ty_unicycle; ty_wheel; ty_wheel; ty_spokes ]
          ; ttc [ ty_unicycle; ty_wheel; ty_tire; ty_tube ]
          ; ttc [ ty_wheel; ty_spokes; ty_bicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_spokes; ty_bicycle; ty_wheel ]
          ; ttc [ ty_wheel; ty_spokes; ty_unicycle; ty_saddle ]
          ; ttc [ ty_wheel; ty_spokes; ty_unicycle; ty_wheel ]
          ; ttc [ ty_wheel; ty_spokes; ty_unicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_spokes; ty_wheel; ty_tire ]
          ; ttc [ ty_wheel; ty_spokes; ty_wheel; ty_spokes ]
          ; ttc [ ty_wheel; ty_spokes; ty_tire; ty_tube ]
          ; ttc [ ty_wheel; ty_tire; ty_bicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_tire; ty_bicycle; ty_wheel ]
          ; ttc [ ty_wheel; ty_tire; ty_unicycle; ty_saddle ]
          ; ttc [ ty_wheel; ty_tire; ty_unicycle; ty_wheel ]
          ; ttc [ ty_wheel; ty_tire; ty_unicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_tire; ty_wheel; ty_tire ]
          ; ttc [ ty_wheel; ty_tire; ty_wheel; ty_spokes ]
          ; ttc [ ty_wheel; ty_tire; ty_tire; ty_tube ]
          ; ttc [ ty_tire; ty_tube; ty_bicycle; ty_pedals ]
          ; ttc [ ty_tire; ty_tube; ty_bicycle; ty_wheel ]
          ; ttc [ ty_tire; ty_tube; ty_unicycle; ty_saddle ]
          ; ttc [ ty_tire; ty_tube; ty_unicycle; ty_wheel ]
          ; ttc [ ty_tire; ty_tube; ty_unicycle; ty_pedals ]
          ; ttc [ ty_tire; ty_tube; ty_wheel; ty_tire ]
          ; ttc [ ty_tire; ty_tube; ty_wheel; ty_spokes ]
          ; ttc [ ty_tire; ty_tube; ty_tire; ty_tube ]
          ]
    ;;

    let typing_hasPart_2_eq_1_2 =
      let equiv =
        Core.Partition.(
          of_list Int.Set.[ singleton 0; of_list [ 1; 2 ]; singleton 3 ])
      in
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_bicycle; ty_wheel; ty_wheel; ty_tire ] ~equiv
          ; ttc [ ty_bicycle; ty_wheel; ty_wheel; ty_spokes ] ~equiv
          ; ttc [ ty_unicycle; ty_wheel; ty_wheel; ty_tire ] ~equiv
          ; ttc [ ty_unicycle; ty_wheel; ty_wheel; ty_spokes ] ~equiv
          ; ttc [ ty_wheel; ty_tire; ty_tire; ty_tube ] ~equiv
          ]
    ;;

    let typing_hasPart_2_eq_1_2_proj_0_4 =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_bicycle; ty_tire ]
          ; ttc [ ty_bicycle; ty_spokes ]
          ; ttc [ ty_unicycle; ty_tire ]
          ; ttc [ ty_unicycle; ty_spokes ]
          ; ttc [ ty_wheel; ty_tube ]
          ]
    ;;

    let typing_hasPartTC =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_bicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_wheel ]
          ; ttc [ ty_bicycle; ty_tire ]
          ; ttc [ ty_bicycle; ty_spokes ]
          ; ttc [ ty_bicycle; ty_tube ]
          ; ttc [ ty_unicycle; ty_saddle ]
          ; ttc [ ty_unicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_tire ]
          ; ttc [ ty_unicycle; ty_spokes ]
          ; ttc [ ty_unicycle; ty_tube ]
          ; ttc [ ty_unicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_tire ]
          ; ttc [ ty_wheel; ty_spokes ]
          ; ttc [ ty_wheel; ty_tube ]
          ; ttc [ ty_tire; ty_tube ]
          ]
    ;;

    let typing_query =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_wheel; ty_tire ]
          ; ttc [ ty_wheel; ty_spokes ]
          ; ttc [ ty_wheel; ty_tube ]
          ]
    ;;

    (* -- Extensional schema -------------------------------------------------*)
    let edb0 =
      Core.Pred.Map.of_alist_exn
        [ pr_cycle, typing_cycle
        ; pr_unicycle, typing_unicycle
        ; pr_bicycle, typing_bicycle
        ; pr_wheel, typing_wheel
        ; pr_tire, typing_tire
        ]
    ;;

    let edb1 =
      Core.Pred.Map.of_alist_exn
        [ pr_cycle, typing_cycle
        ; pr_unicycle, typing_unicycle
        ; pr_bicycle, typing_bicycle
        ; pr_wheel, typing_wheel
        ; pr_tire, typing_tire
        ; pr_hasPart, typing_hasPart
        ]
    ;;

    let edb2 =
      Core.Pred.Map.of_alist_exn
        [ pr_cycle, typing_cycle
        ; pr_unicycle, typing_unicycle
        ; pr_bicycle, typing_bicycle
        ; pr_wheel, typing_wheel
        ; pr_tire, typing_tire
        ; pr_hasPart, typing_hasPart
        ; pr_hasPartTC, typing_hasPartTC
        ]
    ;;

    let edb3 =
      Core.Pred.Map.of_alist_exn
        [ pr_cycle, typing_cycle
        ; pr_unicycle, typing_unicycle
        ; pr_bicycle, typing_bicycle
        ; pr_wheel, typing_wheel
        ; pr_tire, typing_tire
        ; pr_hasPart, typing_hasPart
        ; pr_hasPartTC, typing_hasPartTC
        ; pr_query, typing_query
        ]
    ;;

    (* - Literals used ------------------------------------------------------ *)

    let vx, vy, vz = Core.Term.(var "x", var "y", var "z")
    let ff = Core.Binding.(from_list [ Free; Free ])
    let fff = Core.Binding.(from_list [ Free; Free; Free ])
    let ffff = Core.Binding.(from_list [ Free; Free; Free; Free ])

    let lit_hasPartHead =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_hasPart [ vx; vy ]
    ;;

    let lit_hasPartBody =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_hasPart [ vz; vy ]
    ;;

    let lit_unicycle_12 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_unicycle Core.Term.[ vx; vy; wild (); wild () ]
    ;;

    let lit_unicycle_13 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_unicycle Core.Term.[ vx; wild (); vy; wild () ]
    ;;

    let lit_unicycle_14 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_unicycle Core.Term.[ vx; wild (); wild (); vy ]
    ;;

    let lit_bicycle_12 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_bicycle Core.Term.[ vx; vy; wild (); wild () ]
    ;;

    let lit_bicycle_13 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_bicycle Core.Term.[ vx; wild (); vy; wild () ]
    ;;

    let lit_bicycle_14 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_bicycle Core.Term.[ vx; wild (); wild (); vy ]
    ;;

    let lit_wheel_12 =
      Core.Lit.Adorned.from_raw ~bpatt:fff
      @@ Core.Lit.Raw.lit pr_wheel Core.Term.[ vx; vy; wild () ]
    ;;

    let lit_wheel_13 =
      Core.Lit.Adorned.from_raw ~bpatt:fff
      @@ Core.Lit.Raw.lit pr_wheel Core.Term.[ vx; wild (); vy ]
    ;;

    let lit_wheel_1 =
      Core.Lit.Adorned.from_raw ~bpatt:fff
      @@ Core.Lit.Raw.lit pr_wheel Core.Term.[ vx; wild (); wild () ]
    ;;

    let lit_tire =
      Core.Lit.Adorned.from_raw ~bpatt:ff @@ Core.Lit.Raw.lit pr_tire [ vx; vy ]
    ;;

    let lit_hasPartTC =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_hasPartTC [ vx; vy ]
    ;;

    let lit_hasPartTCBody =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_hasPartTC [ vx; vz ]
    ;;

    let lit_query =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_query [ vx; vy ]
    ;;

    (* -- Strata ---------------------------------------------------------------- *)

    let stratum1 =
      Core.Clause.Adorned.
        [ clause lit_hasPartHead [ lit_unicycle_12 ]
        ; clause lit_hasPartHead [ lit_unicycle_13 ]
        ; clause lit_hasPartHead [ lit_unicycle_14 ]
        ; clause lit_hasPartHead [ lit_bicycle_12 ]
        ; clause lit_hasPartHead [ lit_bicycle_13 ]
        ; clause lit_hasPartHead [ lit_bicycle_14 ]
        ; clause lit_hasPartHead [ lit_wheel_12 ]
        ; clause lit_hasPartHead [ lit_wheel_13 ]
        ; clause lit_hasPartHead [ lit_tire ]
        ]
    ;;

    let stratum2 =
      Core.Clause.Adorned.
        [ clause lit_hasPartTC [ lit_hasPartHead ]
        ; clause lit_hasPartTC [ lit_hasPartTCBody; lit_hasPartBody ]
        ]
    ;;

    let stratum3 =
      Core.Clause.Adorned.[ clause lit_query [ lit_wheel_1; lit_hasPartTC ] ]
    ;;

    (* -- Program --------------------------------------------------------------- *)
    let strata = [ stratum1; stratum2; stratum3 ]
    let queries = [ pr_query ]
    let prog = Core.Program.Stratified.{ strata; queries }
  end
end
